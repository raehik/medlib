module Medlib.Action.FFmpeg
  ( Cfg(..)
  , compareStoredHash
  , hashAndTranscode
  ) where

import           Medlib.Util.Process
import           Medlib.Util.FileProcess

import           Control.Monad.IO.Class
import qualified Data.Text                      as Text
import           Data.Text                      ( Text )
import           System.Exit                    ( ExitCode(..) )

import           FP

data Cfg = Cfg
  { cfgHashTagName :: String
  , cfgHasher      :: String
  , cfgFFprobe     :: String
  , cfgFFmpeg      :: String
  }

-- | Given the filename of a track, and the filename of a track generated from
--   that track, check that the hash stored inside the generated track equals
--   the hash of the original track.
compareStoredHash
    :: MonadIO m => Cfg -> FPF -> FPF -> m (Either String Bool)
compareStoredHash cfg fOrigTrack fGenTrack = do
    ffprobeGetTag (cfgFFprobe cfg) (cfgHashTagName cfg) fGenTrack >>= \case
      Left ec -> err ec "getting stored hash"
      Right storedHash -> do
        hashFile (cfgHasher cfg) fOrigTrack >>= \case
          Left ec -> err ec "hashing original track"
          Right originalHash -> return $ Right $ storedHash == originalHash
  where
    err ec attemptedAction =
        return $ Left $ "error code " <> show ec <> " while " <> attemptedAction

hashAndTranscode :: MonadIO m => Cfg -> String -> FPF -> FPF -> m (Maybe String)
hashAndTranscode cfg quality fIn fOut = do
    hashFile (cfgHasher cfg) fIn >>= \case
      Left ec -> err ec "hashing original track"
      Right originalHash -> do
        runProcessSilent (cfgFFmpeg cfg) (ffmpegArgs (Text.unpack originalHash)) >>= \case
          ExitFailure ec -> err ec "transcoding"
          ExitSuccess    -> return Nothing
  where
    err ec attemptedAction =
        return $ Just $ "error code " <> show ec <> " while " <> attemptedAction
    ffmpegArgs hash =
        [ "-n", "-i", fIn, "-vn", "-q:a", quality
        , "-metadata", cfgHashTagName cfg<>"="<>hash
        , fOut ]

--------------------------------------------------------------------------------

-- | Read the given tag from a music file using ffprobe.
--
-- Haha, I have to double-fmap it in order to get past the 'm', to the 'Either'.
-- Now I understand that pattern!
ffprobeGetTag :: MonadIO m => String -> String -> FPF -> m (Either Int Text)
ffprobeGetTag ffprobeExe tagName fp = fmap (head . Text.words) <$> readProcToText ffprobeExe (fp : ffprobeArgs)
  where
    ffprobeArgs = ["-show_entries", "stream_tags="<>tagName, "-of", "csv=p=0"]
