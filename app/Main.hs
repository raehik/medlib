module Main where

import qualified CLI as CLI
import           Config

import qualified Medlib.Action           as Action
import qualified Medlib.Util.FileProcess as FileProcess
import qualified Medlib.Util.File        as File

import           Optics
import           Data.Generics.Product.Any
import           Control.Monad.IO.Class

aFFmpegCfg :: CFFmpeg -> CHasher -> Action.FFmpegCfg
aFFmpegCfg cFFmpeg cHasher = Action.FFmpegCfg
  { Action.ffmpegCfgHashTagName = cFFmpegHashTag cFFmpeg
  , Action.ffmpegCfgHashFile    = hashFile (cHasherExe cHasher)
  , Action.ffmpegCfgFFprobe     = cFFmpegFFprobe cFFmpeg
  , Action.ffmpegCfgFFmpeg      = cFFmpegFFmpeg  cFFmpeg
  , Action.ffmpegCfgQuality     = cFFmpegQuality cFFmpeg
  } where hashFile hasher fp = fmap (fst . head) <$> FileProcess.hashFiles hasher [fp]

main :: IO ()
main = CLI.parseOpts >>= print

{-
runCmdMakePortable :: MonadIO m => CCmdMakePortable -> m ()
runCmdMakePortable cfg = do
    qStatus <- liftIO newTQueueIO
    aStatus <- liftIO $ async $ TODO qStatus
    aDelegator <- liftIO $ async $ delegator sLibFiles
    liftIO $ wait aDelegator
    liftIO $ wait aLogger
  where
    sLibFiles = File.deepTraverseDir traverseDirPred lib
    traverseDirPred (_, d) = d `notElem` traverseSkipDirs
    traverseSkipDirs = cfg & cCmdMakePortableCTraverser & cTraverserSkipDirs
    mJobsCPU = cfg & cCmdMakePortableCScheduler & cSchedulerCPUJobs
    jobsIO   = cfg & cCmdMakePortableCScheduler & cSchedulerIOJobs
    lib      = cfg & cCmdMakePortableCLibrarySrc
    libDest  = cfg & cCmdMakePortableCLibraryDest
-}
