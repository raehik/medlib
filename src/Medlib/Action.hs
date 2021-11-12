{- TODO
* Text better than String for textual comparisons? Probs tbh.
-}

module Medlib.Action where

import           Medlib.Util.Process
import           Medlib.Util.FileProcess

import           Control.Monad.IO.Class
import qualified Data.Text                      as Text
import           Data.Text                      ( Text )
import           System.Exit                    ( ExitCode(..) )

type FPF = FilePath
type FPD = FilePath

data FFmpegCfg = FFmpegCfg
  { ffmpegCfgHashTagName :: String
  , ffmpegCfgHashFile    :: FPF -> IO (Either Int Text)
  , ffmpegCfgFFprobe     :: String
  , ffmpegCfgFFmpeg      :: String
  , ffmpegCfgQuality     :: String
  }

ffmpegB3 :: String -> String -> String -> String -> FFmpegCfg
ffmpegB3 b3sumExe ffprobeExe ffmpegExe qual = FFmpegCfg
  { ffmpegCfgHashTagName = "MedlibOriginalHashBlake3"
  , ffmpegCfgHashFile    = hashFile
  , ffmpegCfgFFprobe     = ffprobeExe
  , ffmpegCfgFFmpeg      = ffmpegExe
  , ffmpegCfgQuality     = qual
  } where hashFile fp = fmap (fst . head) <$> hashFiles b3sumExe [fp]

-- | Given the filename of a track, and the filename of a track generated from
--   that track, check that the hash stored inside the generated track equals
--   the hash of the original track.
compareTrackStoredHash
    :: MonadIO m => FFmpegCfg -> FPF -> FPF -> m (Either String Bool)
compareTrackStoredHash cfg fOrigTrack fGenTrack = do
    ffprobeGetTag (ffmpegCfgFFprobe cfg) (ffmpegCfgHashTagName cfg) fGenTrack >>= \case
      Left ec -> err ec "getting stored hash"
      Right storedHash -> do
        (liftIO $ ffmpegCfgHashFile cfg fOrigTrack) >>= \case
          Left ec -> err ec "hashing original track"
          Right originalHash -> return $ Right $ storedHash == originalHash
  where
    err ec attemptedAction =
        return $ Left $ "error code " <> show ec <> " while " <> attemptedAction

compareFileHashes
    :: MonadIO m => FPF -> [FPF] -> m (Either String Bool)
compareFileHashes hasher fs = hashFiles hasher fs >>= \case
  Left ec -> return $ Left $ "error code " <> show ec <> " while hashing filelist"
  Right hashes ->
    case hashes of
      []   -> return $ Right True
      h:hs -> return $ Right $ all (== snd h) (map snd hs)

-- | Read the given tag from a music file using ffprobe.
--
-- Haha, I have to double-fmap it in order to get past the 'm', to the 'Either'.
-- Now I understand that pattern!
ffprobeGetTag :: MonadIO m => String -> String -> FPF -> m (Either Int Text)
ffprobeGetTag ffprobeExe tagName fp = fmap (head . Text.words) <$> readProcToText ffprobeExe (fp : ffprobeArgs)
  where
    ffprobeArgs = ["-show_entries", "stream_tags="<>tagName, "-of", "csv=p=0"]

hashAndTranscode :: MonadIO m => FFmpegCfg -> FPF -> FPF -> m (Maybe String)
hashAndTranscode cfg fIn fOut = do
    (liftIO $ ffmpegCfgHashFile cfg fIn) >>= \case
      Left ec -> err ec "hashing original track"
      Right originalHash -> do
        runProcessSilent (ffmpegCfgFFmpeg cfg) (ffmpegArgs (Text.unpack originalHash)) >>= \case
          ExitFailure ec -> err ec "transcoding"
          ExitSuccess    -> return Nothing
  where
    err ec attemptedAction =
        return $ Just $ "error code " <> show ec <> " while " <> attemptedAction
    ffmpegArgs hash =
        [ "-n", "-i", fIn, "-vn", "-q:a", ffmpegCfgQuality cfg
        , "-metadata", ffmpegCfgHashTagName cfg<>"="<>hash
        , fOut ]
