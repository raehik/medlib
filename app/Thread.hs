{-
TODO not ideal how I stuff everything into 'Job', like even the source file,
even though it's already baked into the action. The scheduler needs it for the
status update. I should instead prepare the job specially in the delegator, so
it asks for a slot and then can write its own updates!

TODO I do the above now. But I still use Job. Bit silly.
-}

{-# LANGUAGE DeriveAnyClass #-}

module Thread where

import qualified Medlib.Map                     as MedlibMap
import qualified Medlib.Map.Status              as MapStatus
import qualified Medlib.Action.FFmpeg           as ActFFmpeg

import           Config
import           Util

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMQueue
import           Control.Concurrent.Async

import           Control.Exception

import qualified Data.List                      as List
import qualified Data.Map                       as Map
import           Data.Map                       ( Map )
import qualified Data.Set                       as Set
import           Data.Set                       ( Set )
import           GHC.Generics
import qualified System.Directory               as Dir
import qualified Medlib.Util.FileProcess        as FileProcess
import qualified System.FilePath                as FilePath
import           System.FilePath                ( (</>) )
import           Data.Function                  ( (&) )

import           System.Console.Regions

data Job = Job
  { jobAction    :: IO (Maybe String)
  , jobWriteFile :: (FilePath, FilePath)
  , jobSrc       :: (FilePath, FilePath)
  , jobPool      :: MedlibMap.ResourceBound
  , jobType      :: MedlibMap.Op
  } deriving (Generic)

-- TODO use cfg
delegator
    :: MonadIO m
    => CCmdMakePortable -> TMQueue (FilePath, FilePath) -> TQueue MedlibMap.Update
    -> m ()
delegator cfg qFiles qStatus = do
    (qCPU, tCPU) <- createPool MedlibMap.ResourceBoundCPU 1
    (qIO,  tIO)  <- createPool MedlibMap.ResourceBoundIO  1
    delegate cfg qFiles qStatus qCPU qIO
    report MedlibMap.UpdateFullyTraversed
    stm $ closeTMQueue qCPU
    stm $ closeTMQueue qIO
    liftIO $ wait tCPU
    liftIO $ wait tIO
  where
    report = stm . writeTQueue qStatus
    createPool p n = do
        let reportDone = stm $ writeTQueue qStatus $ MedlibMap.UpdatePool p MedlibMap.PoolUpdateClosed
        q <- liftIO newTMQueueIO
        a <- liftIO $ async $ scheduler n q reportDone
        return (q, a)

delegate
    :: MonadIO m
    => CCmdMakePortable -> TMQueue (FilePath, FilePath) -> TQueue MedlibMap.Update
    -> TMQueue (Int -> IO (Maybe String)) -> TMQueue (Int -> IO (Maybe String))
    -> m ()
delegate cfg qFiles qStatus qCPU qIO = nextFile
  where
    nextFile = stm (readTMQueue qFiles) >>= \case
                 Nothing -> return ()
                 Just fp -> delegateFile fp
    delegateFile fp@(fpd, fpf) = do
        job <- determineJob cfg fp
        let pool = jobPool job
            job' = wrapJob qStatus (jobAction job) (fpd </> fpf) (jobType job) pool
            poolQueue = case pool of
                          MedlibMap.ResourceBoundCPU -> qCPU
                          MedlibMap.ResourceBoundIO  -> qIO
        liftIO $ Dir.createDirectoryIfMissing True $ rootDest </> fst (jobWriteFile job)
        stm $ writeTMQueue poolQueue job'
        stm $ writeTQueue qStatus $ MedlibMap.UpdatePool pool $ MedlibMap.PoolUpdateQueued
        nextFile
    rootDest = cfg & cCmdMakePortableCLibraryDest & cLibraryRoot


-- TODO curry issues with MonadIO...
wrapJob
    :: TQueue MedlibMap.Update -> IO a
    -> FilePath -> MedlibMap.Op -> MedlibMap.ResourceBound -> Int
    -> IO a
wrapJob q j fp op p s = do
    report $ MedlibMap.SlotBusy fp op
    jOut <- liftIO j
    report $ MedlibMap.SlotIdle
    return jOut
  where
    report ss =
        let update = MedlibMap.UpdatePool p $ MedlibMap.PoolUpdateSlot s ss
         in liftIO $ atomically $ writeTQueue q update

data Err = ErrJobFailed String
  deriving          (Eq, Show)
  deriving anyclass Exception

scheduler
    :: MonadIO m
    => Int -> TMQueue (Int -> IO (Maybe String)) -> IO ()
    -> m ()
scheduler numSlots q reportDone = go Map.empty
  where
    go threads = do
        stm (readTMQueue q) >>= \case
          Just job -> do
            (threads', slot) <- do
                if   Map.size threads >= numSlots
                then waitAnyMap threads
                else return (threads, minNotIn (Set.fromList (Map.elems threads)))
            thread <- liftIO $ async $ job slot
            go $ Map.insert thread slot threads
          Nothing  -> do
            waitAllMap threads
            liftIO reportDone
    -- | 'waitAny' on the keys of an async map, delete the winner from the map
    --   and return its value.
    --
    -- Note that 'waitAny' should return the earliest completed async.
    --
    -- TODO ALSO THROWS AN EXECEPTION IF A HANDLED ERROR WAS RETURNED
    waitAnyMap m = do
        (a, mErr) <- liftIO $ waitAny $ Map.keys m
        let Just v = Map.lookup a m -- safe, no choice due to waitAny
            m'     = Map.delete a m
        case mErr of
          Nothing     -> return (m', v)
          Just errStr -> liftIO $ throwIO $ ErrJobFailed errStr
    -- | Wait for all asyncs in a map to finish.
    waitAllMap m | Map.null m = return ()
                 | otherwise  = waitAnyMap m >>= \(m', _) -> waitAllMap m'

-- TODO use cfg
status :: MonadIO m => CConcurrentLogger -> TQueue MedlibMap.Update -> m ()
status _ q = liftIO $ displayConsoleRegions $ do
    cr <- liftIO $ openConsoleRegion Linear
    go cr MapStatus.statusDef
  where
    go cr s = case MapStatus.indicatesFinished s of
             True  -> liftIO $ finishConsoleRegion cr $ show' s
             False -> do
               upd <- stm $ readTQueue q
               let s' = MapStatus.processUpdate s upd
               --liftIO $ print s'
               liftIO $ setConsoleRegion cr $ show' s'
               go cr s'
    show' = MapStatus.showStatusEachSlot

-- | Determine the job to run for the given library file.
--
-- The decision is made as follows:
--
--   * If the extension indicates the file is a track to be transcoded:
--     * If the destination file already exists, look for a special hash stored
--       in the track metadata. Return a job that attempts to compare this hash
--       to the source file's hash.
--     * Otherwise, return a job to transcode the file.
--   * Otherwise, we treat it as a plain file to be copied.
--     * If the destination file already exists, return a job that attempts to
--       compare the source and destination file hashes.
--     * Otherwise, return a job to copy the file.
--
determineJob :: MonadIO m => CCmdMakePortable -> (FilePath, FilePath) -> m Job
determineJob cfg fp@(fpd, fpf) = do
    case FilePath.takeExtension fpf of
      '.':ext -> case Map.lookup ext (cfg & cTranscoder & mappings) of
                   Nothing      -> determineJobCp
                   Just mapping -> determineJobTranscode mapping
      _       -> determineJobCp
  where
    determineJobTranscode mapping = do
        let fpfDest = FilePath.replaceExtension fpf ('.':extension mapping)
            fDest = rootDest </> fpd </> fpfDest
        liftIO (Dir.doesFileExist fDest) >>= \case
          True  -> return $ Job { jobAction    = jobCompareStoredHash fDest
                                , jobWriteFile = (fpd, fpfDest)
                                , jobSrc       = fp
                                , jobPool      = MedlibMap.ResourceBoundCPU
                                , jobType      = MedlibMap.OpCompareStoredHash }
          False -> return $ Job { jobAction    = jobTranscode (quality mapping) fDest
                                , jobWriteFile = (fpd, fpfDest)
                                , jobSrc       = fp
                                , jobPool      = MedlibMap.ResourceBoundCPU
                                , jobType      = MedlibMap.OpTranscode }
    determineJobCp = do
        let fDest = rootDest </> fpd </> fpf
        liftIO (Dir.doesFileExist fDest) >>= \case
          True  -> return $ Job { jobAction    = jobCompareHashes fDest
                                , jobWriteFile = fp
                                , jobSrc       = fp
                                , jobPool      = MedlibMap.ResourceBoundCPU
                                , jobType      = MedlibMap.OpCompareHashes }
          False -> return $ Job { jobAction    = jobCp fDest
                                , jobWriteFile = fp
                                , jobSrc       = fp
                                , jobPool      = MedlibMap.ResourceBoundIO
                                , jobType      = MedlibMap.OpCopy }
    rootSrc  = cfg & cCmdMakePortableCLibrarySrc  & cLibraryRoot
    rootDest = cfg & cCmdMakePortableCLibraryDest & cLibraryRoot
    hasher   = cfg & cCmdMakePortableCHasher & cHasherExe
    fSrc  = rootSrc  </> fpd </> fpf
    jobCp fDest = do
        Dir.copyFile fSrc fDest
        return Nothing
    jobCompareHashes fDest = do
        let fs = [fSrc, fDest]
        FileProcess.compareFileHashes hasher fs >>= \case
          Left  errStr   -> return $ Just errStr
          Right success  -> return $ if   success
                                     then Nothing
                                     else Just $  "hashes didn't match: "
                                               <> List.intercalate ", " fs
    jobTranscode quality fDest = ActFFmpeg.hashAndTranscode ffmpegCfg quality fSrc fDest
    jobCompareStoredHash fDest = do
        ActFFmpeg.compareStoredHash ffmpegCfg fSrc fDest >>= \case
          Left  errStr  -> return $ Just errStr
          Right success ->
            if success
            then return Nothing
            else return $ Just $  "stored hash in "        <> fDest
                               <> " didn't match hash of " <> fSrc
    ffmpegCfg = aFFmpegCfg (cTranscoder cfg) (cfg & cCmdMakePortableCHasher)

aFFmpegCfg :: CTranscoder -> CHasher -> ActFFmpeg.Cfg
aFFmpegCfg cTranscoder cHasher =
    ActFFmpeg.Cfg { ActFFmpeg.cfgHasher      = cHasherExe cHasher
                  , ActFFmpeg.cfgHashTagName = hashTag cTranscoder
                  , ActFFmpeg.cfgFFprobe     = ffprobe cTranscoder
                  , ActFFmpeg.cfgFFmpeg      = ffmpeg  cTranscoder
                  }

-- | Get the smallest integer x where x >= 1 and x is not in the given set. The
--   set must only have members >= 1.
minNotIn :: Set Int -> Int
minNotIn = go 1
  where go cur s = case Set.lookupMin s of
                     Nothing      -> cur
                     Just nextMin -> if   nextMin /= cur
                                     then cur
                                     else go (nextMin+1) (Set.delete nextMin s)
