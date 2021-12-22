module Medlib.Job.FFmpeg
  ( Cfg(..)
  , compareStoredHash
  , compareStoredSize
  , transcodeStoreOrigHash
  , transcodeStoreOrigSize
  ) where

import           Medlib.Util.Process
import           Medlib.Util.FileProcess

import           Control.Monad.IO.Class
import qualified Data.Text                      as Text
import           Data.Text                      ( Text )
import qualified Data.Map                       as Map
import           Data.Map                       ( Map )
import           System.Exit                    ( ExitCode(..) )

import           FP

tagMapAsMetadataArgs :: Map Text Text -> [String]
tagMapAsMetadataArgs = map (uncurry tagArg) . Map.assocs
  where tagArg t v = Text.unpack t <> "=" <> Text.unpack v

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

compareStoredSize
    :: MonadIO m => FPF -> String -> FPF -> FPF -> m (Either String Bool)
compareStoredSize ffprobe tagName fOrigTrack fGenTrack = do
    ffprobeGetTag ffprobe tagName fGenTrack >>= \case
      Left ec -> err ec "getting stored size"
      Right storedSize -> do
        size <- getFileSize fOrigTrack
        return $ Right $ size == storedSize
  where
    err ec attemptedAction =
        return $ Left $ "error code " <> show ec <> " while " <> attemptedAction

transcodeStoreOrigHash :: MonadIO m => Cfg -> String -> FPF -> FPF -> m (Maybe String)
transcodeStoreOrigHash cfg quality fIn fOut = do
    hashFile (cfgHasher cfg) fIn >>= \case
      Left ec -> err ec "hashing original track"
      Right originalHash -> do
        transcode
            (cfgFFmpeg cfg)
            quality
            (cfgHashTagName cfg)
            (Text.unpack originalHash)
            fIn fOut >>= \case
          ExitFailure ec -> err ec "transcoding"
          ExitSuccess    -> return Nothing
  where
    err ec attemptedAction =
        return $ Just $ "error code " <> show ec <> " while " <> attemptedAction

transcodeStoreOrigSize :: MonadIO m => FPF -> String -> String -> FPF -> FPF -> m (Maybe String)
transcodeStoreOrigSize ffmpeg quality tagName fIn fOut = do
    size <- getFileSize fIn
    transcode ffmpeg quality tagName (Text.unpack size) fIn fOut >>= \case
      ExitFailure ec -> err ec "transcoding"
      ExitSuccess    -> return Nothing
  where
    err ec attemptedAction =
        return $ Just $ "error code " <> show ec <> " while " <> attemptedAction

--------------------------------------------------------------------------------

-- | Transcode with some set stuff and a spare tag.
transcode :: MonadIO m => FPF -> String -> String -> String -> FPF -> FPF -> m ExitCode
transcode ffmpeg quality tagName tagValue fIn fOut = runProcessSilent ffmpeg args
  where args = [ "-n", "-i", fIn, "-vn", "-q:a", quality
               , "-metadata", tagName<>"="<>tagValue
               , fOut ]

-- | Read the given tag from a music file using ffprobe.
--
-- Haha, I have to double-fmap it in order to get past the 'm', to the 'Either'.
-- Now I understand that pattern!
ffprobeGetTag :: MonadIO m => String -> String -> FPF -> m (Either Int Text)
ffprobeGetTag ffprobeExe tagName fp = fmap (head . Text.words) <$> readProcToText ffprobeExe (fp : ffprobeArgs)
  where
    ffprobeArgs = ["-show_entries", "stream_tags="<>tagName, "-of", "csv=p=0"]
