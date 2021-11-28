module Main where

import qualified CLI as CLI
import           Config
import           Util
import qualified Thread                  as Thread

import qualified Medlib.Util.File        as File
import qualified Medlib.Map              as MedlibMap

import           Optics
import           Control.Monad.IO.Class
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMQueue
import           GHC.Conc                       ( getNumCapabilities )
import qualified Streaming.Prelude              as S
import           Streaming.Prelude              ( Stream, Of )

main :: IO ()
main = CLI.parseOpts >>= \case
  CmdMakePortable cfg -> runCmdMakePortable cfg

runCmdMakePortable :: MonadIO m => CCmdMakePortable -> m ()
runCmdMakePortable cfg = do
    getPoolSize <- makePoolSizer (cScheduler cfg)
    qStatus <- liftIO newTQueueIO
    aStatus <- liftIO $ async $ Thread.status (cCmdMakePortableCConcurrentLogger cfg) qStatus
    qLibFiles <- liftIO newTMQueueIO
    aLibFiles <- liftIO $ async $ streamToTMQueue sLibFiles qLibFiles
    aDelegator <- liftIO $ async $ Thread.delegator cfg getPoolSize qLibFiles qStatus
    liftIO $ wait aLibFiles
    liftIO $ wait aDelegator
    liftIO $ wait aStatus
  where
    sLibFiles = File.deepTraverseDir traverseDirPred (cLibraryRoot lib)
    traverseDirPred (_, d) = d `notElem` traverseSkipDirs
    traverseSkipDirs = cfg & cCmdMakePortableCTraverser & cTraverserSkipDirs
    lib      = cfg & cCmdMakePortableCLibrarySrc

-- | Consume a 'Stream' to a 'TMQueue' and close the queue once finished.
streamToTMQueue :: MonadIO m => Stream (Of a) m () -> TMQueue a -> m ()
streamToTMQueue s q = do
    S.mapM_ (stm . writeTMQueue q) s
    stm $ closeTMQueue q

makePoolSizer :: MonadIO m => CScheduler -> m (MedlibMap.ResourceBound -> Int)
makePoolSizer cfg = do
    sizeCPU <- case cSchedulerCPUJobs cfg of
                 Just sizeCPU -> return sizeCPU
                 Nothing      -> liftIO getNumCapabilities
    return $ go sizeCPU (cSchedulerIOJobs cfg)
  where
    go sizeCPU sizeIO = \case
      MedlibMap.ResourceBoundCPU -> sizeCPU
      MedlibMap.ResourceBoundIO  -> sizeIO
