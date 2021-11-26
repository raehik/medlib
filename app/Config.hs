module Config where

import           GHC.Generics
import           Data.Map       ( Map )

data Command
  = CmdMakePortable CCmdMakePortable
    deriving (Eq, Show)

data CCmdMakePortable = CCmdMakePortable
  { cCmdMakePortableCTraverser :: CTraverser
  , cCmdMakePortableCScheduler :: CScheduler
  , cTranscoder :: CTranscoder
  , cCmdMakePortableCHasher :: CHasher
  , cCmdMakePortableCConcurrentLogger :: CConcurrentLogger
  , cCmdMakePortableCLibrarySrc :: CLibrary
  , cCmdMakePortableCLibraryDest :: CLibrary
  } deriving (Eq, Show, Generic)

data CLibrary = CLibrary
  { cLibraryRoot :: String
  } deriving (Eq, Show)

data CTraverser = CTraverser
  { cTraverserSkipDirs :: [String]
  } deriving (Eq, Show)

data CScheduler = CScheduler
  { cSchedulerCPUJobs :: Maybe Int
  , cSchedulerIOJobs  :: Int
  } deriving (Eq, Show)

data CTranscoder = CTranscoder
  { hashTag  :: String
  , ffmpeg   :: String
  , ffprobe  :: String
  , mappings :: Map String CTranscoderMapping
  } deriving (Eq, Show)

data CTranscoderMapping = CTranscoderMapping
  { extension :: String
  , quality   :: String
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
