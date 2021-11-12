module Config where

import           GHC.Generics

data Command
  = CmdMakePortable CCmdMakePortable
    deriving (Eq, Show)

data CCmdMakePortable = CCmdMakePortable CTraverser CScheduler CTranscoder CFFmpeg CHasher CConcurrentLogger
    deriving (Eq, Show, Generic)

data CTraverser = CTraverser
  { cTraverserSkipDirs :: [String]
  } deriving (Eq, Show)

data CScheduler = CScheduler
  { cSchedulerCPUJobs :: Maybe Int
  , cSchedulerIOJobs  :: Int
  } deriving (Eq, Show)

data CTranscoder = CTranscoder
  { cTranscoderExtPairs :: [(String, String)]
  } deriving (Eq, Show)

data CFFmpeg = CFFmpeg
  { cFFmpegHashTag :: String
  , cFFmpegFFmpeg  :: String
  , cFFmpegFFprobe :: String
  , cFFmpegQuality :: String
  } deriving (Eq, Show)

data CHasher = CHasher
  { cHasherExe :: String
  } deriving (Eq, Show)

-- | Config for where concurrent logging (multiple jobs running concurrently and
--   reporting results) is involved.
data CConcurrentLogger = CConcurrentLogger
  { cConcurrentLoggerOutput :: CConcurrentLogMethod
  } deriving (Eq, Show)

data CConcurrentLogMethod
  = CConcurrentLogMethodDynamicPerJob
  | CConcurrentLogMethodDynamicPerJobCompact
  | CConcurrentLogMethodScrolling
    deriving (Eq, Show)
