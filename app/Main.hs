module Main where

import qualified CLI as CLI
import           Config
import           Util
import qualified Thread                  as Thread

import qualified Medlib.Util.File        as File

import           Optics
import           Data.Generics.Product.Any
import           Control.Monad.IO.Class
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMQueue
import qualified Streaming.Prelude              as S
import           Streaming.Prelude              ( Stream, Of )

main :: IO ()
main = CLI.parseOpts >>= \case
  CmdMakePortable cfg -> runCmdMakePortable cfg

runCmdMakePortable :: MonadIO m => CCmdMakePortable -> m ()
runCmdMakePortable cfg = do
    qStatus <- liftIO newTQueueIO
    aStatus <- liftIO $ async $ Thread.status (cCmdMakePortableCConcurrentLogger cfg) qStatus
    qLibFiles <- liftIO newTMQueueIO
    aLibFiles <- liftIO $ async $ streamToTMQueue sLibFiles qLibFiles
    aDelegator <- liftIO $ async $ Thread.delegator cfg qLibFiles qStatus
    liftIO $ wait aLibFiles
    liftIO $ wait aDelegator
    liftIO $ wait aStatus
  where
    sLibFiles = File.deepTraverseDir traverseDirPred (cLibraryRoot lib)
    traverseDirPred (_, d) = d `notElem` traverseSkipDirs
    traverseSkipDirs = cfg & cCmdMakePortableCTraverser & cTraverserSkipDirs
    mJobsCPU = cfg & cCmdMakePortableCScheduler & cSchedulerCPUJobs
    jobsIO   = cfg & cCmdMakePortableCScheduler & cSchedulerIOJobs
    lib      = cfg & cCmdMakePortableCLibrarySrc
    libDest  = cfg & cCmdMakePortableCLibraryDest

-- | Consume a 'Stream' to a 'TMQueue' and close the queue once finished.
streamToTMQueue :: MonadIO m => Stream (Of a) m () -> TMQueue a -> m ()
streamToTMQueue s q = do
    S.mapM_ (stm . writeTMQueue q) s
    stm $ closeTMQueue q
