-- | As I hoped/expected, I managed to write a lot of highly generic concurrency
--   code while designing my medlib mapper. The internals are essentially
--   scattered concurrency primitives over 'TMQueue's.
--
-- TODO: Try rewriting the traversal to use the streaming package instead. Then
-- I can decide whether to go lazy (= low mem, no (useful) file counting) or
-- fully evaluate (= high mem, file counting -- current).

module Medlib.Map.Internal where

import           Medlib.Utils.Files

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Concurrent.Async
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map
import           Data.Map                       ( Map )
import qualified Streaming.Prelude              as S
import           Streaming.Prelude              ( Stream, Of )

delegateStreamToPoolQueues
    :: (MonadIO m, Eq pool, Ord pool)
    => (Map pool (TMQueue out))
    -> (itm -> (out, pool))
    -> TMQueue out
    -> Stream (Of itm) m ()
    -> m ()
delegateStreamToPoolQueues poolDataMap process qNonMatch s = do
    S.mapM_ processItm s
    mapM_ closePoolQueue $ Map.elems poolDataMap
  where
    closePoolQueue = liftIO . atomically . closeTMQueue
    processItm itm = do
        let (out, pool) = process itm
            qForward   = Map.findWithDefault qNonMatch pool poolDataMap
        liftIO $ atomically $ writeTMQueue qForward out

-- | Run jobs from a 'TMQueue' concurrently using a bounded thread pool.
--
-- The scheduler will attempt to run as many jobs as the thread pool supports.
-- If it's full, it waits for any job to finish, then schedules. Once the queue
-- closes, it waits for all jobs to finish, then finally returns.
scheduleIOJobQueue :: MonadIO m => Int -> (itm -> IO ()) -> TMQueue itm -> m ()
scheduleIOJobQueue poolSizeMax action queue = go Set.empty
  where
    go pool = do
        liftIO (atomically (readTMQueue queue)) >>= \case
          Just itm -> do
            pool' <- do
                if   Set.size pool >= poolSizeMax
                then waitAnySet pool
                else return pool
            a <- liftIO $ async $ action itm
            go $ Set.insert a pool'
          Nothing -> do
            waitAll pool
            return ()
    waitAll pool
     | Set.null pool = return ()
     | otherwise = waitAnySet pool >>= waitAll
    -- | waitAny and remove the winner from the set
    waitAnySet s = do
        (a', _) <- liftIO $ waitAny $ Set.toList s
        return $ Set.delete a' s

--------------------------------------------------------------------------------

deepTraverseDirAndSTMQueueFiles
    :: MonadIO m
    => ((FilePathD, FilePathD) -> Bool)
    -> TMQueue (FilePathD, FilePathF) -> FilePath
    -> m ()
deepTraverseDirAndSTMQueueFiles traverseDirPred q root = do
    S.mapM_ writeToQueue traverseStream
    liftIO $ atomically $ closeTMQueue q
  where
    traverseStream = deepTraverseDir traverseDirPred root
    writeToQueue = liftIO . atomically . writeTMQueue q

-- | Repropagate each entry in a 'TMQueue' to another value-indexed queue.
--
-- I use a 'Map' because I'm not sure about how passing a function may work
-- here. But since a 'Map' can't fully replace a total function, you must also
-- pass a "fallback" queue, which is used for index values not present in the
-- value-indexed queue map.
mapDivideQueue
    :: (Eq idx, Ord idx, MonadIO m)
    => (itm -> (out, idx)) -> Map idx (TMQueue out) -> TMQueue out
    -> TMQueue itm -> m ()
mapDivideQueue process idxMap qNonMatch q = go
  where
    go = do
        liftIO (atomically (readTMQueue q)) >>= \case
          Just itm -> do
            let (out, idx) = process itm
                qForward   = Map.findWithDefault qNonMatch idx idxMap
            liftIO $ atomically $ writeTMQueue qForward out
            go
          Nothing -> closeQs $ qNonMatch : Map.elems idxMap
    closeQs = \case
      []      -> return ()
      (q':qs) -> do
        liftIO $ atomically $ closeTMQueue q'
        closeQs qs
