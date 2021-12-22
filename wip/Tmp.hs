module Tmp where

-- -q:a is -qscale:a where a is a stream specifier for all audio streams.
data FFmpegAudioTranscodeCmd = FFmpegAudioTranscodeCmd
  { exe          :: FilePath
  , audioQuality :: Text
  , tags         :: Map Text Text
  , inFile       :: FilePath
  , outFile      :: FilePath
  } deriving (Eq, Show)


