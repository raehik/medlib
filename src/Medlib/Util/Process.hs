module Medlib.Util.Process where

import           Control.Monad.IO.Class
import           Control.Exception      ( throwIO )
import           System.Exit            ( ExitCode(..) )
import           System.Process.Typed
import qualified Data.Text.Encoding     as Text
import           Data.Text              ( Text )
import qualified Data.ByteString.Lazy   as BL

-- | Run a process and return its stdout as a 'Text'.
readProcToText :: MonadIO m => FilePath -> [String] -> m (Either Int Text)
readProcToText cmd args = do
    (ec, stdoutBs) <- readProcessDetached cmd args
    case ec of
      ExitFailure ec' -> return $ Left ec'
      ExitSuccess -> do
        case Text.decodeUtf8' (BL.toStrict stdoutBs) of
          Left err -> liftIO $ throwIO err -- pretend it doesn't exist.
          Right stdoutText -> return $ Right stdoutText

-- | Run a process synchronously and throw away all streams.
runProcessSilent :: MonadIO m => FilePath -> [String] -> m ExitCode
runProcessSilent cmd args = liftIO $ runProcess $ silenceProcess $ proc cmd args
  where silenceProcess = setStderr nullStream . setStdin nullStream . setStdout nullStream

-- | Run a process synchronously with all streams detached, and return stdout
--   and the exit code.
readProcessDetached :: MonadIO m => FilePath -> [String] -> m (ExitCode, BL.ByteString)
readProcessDetached cmd args = liftIO $ readProcessStdout $ setStderr nullStream $ proc cmd args
