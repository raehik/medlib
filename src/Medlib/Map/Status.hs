{-
Data and functions for tracking library mapping status, and printing it out.

Pool closure isn't very safe. But it seems a smart approach.
-}

{-# LANGUAGE DataKinds #-}

module Medlib.Map.Status where

import           GHC.Generics
import           Data.Function
import qualified Data.Map       as Map
import           Data.Map       ( Map )
import qualified Data.List      as List
import           Optics
import           Data.Generics.Product.Any

data LibraryMapStatus = LibraryMapStatus
  { lmsFullyTraversed :: Bool
  , pools :: Map Pool WorkerPoolStatus
  } deriving (Eq, Show, Generic)

data WorkerPoolStatus = WorkerPoolStatus
  { scheduledJobs :: Int
  , wpsProcessedJobs :: Int
  , closed           :: Bool
  , workers :: Map Int WorkerStatus
  } deriving (Eq, Show, Generic, Ord)

data WorkerStatus
  = WorkerBusy String LibraryFileOp
  | WorkerIdle
    deriving (Eq, Show, Generic, Ord)

data LibraryFileOp
  = LFOTranscode
  | LFOCompareStoredHash
  | LFOCopy
  | LFOCompareHashes
    deriving (Eq, Show, Generic, Ord)

lfoVerbing :: LibraryFileOp -> String
lfoVerbing = \case
  LFOTranscode         -> "transcoding"
  LFOCompareStoredHash -> "checking transcoded stored hash"
  LFOCopy              -> "copying"
  LFOCompareHashes     -> "checking hashes"

lfoID :: LibraryFileOp -> Char
lfoID = \case
  LFOTranscode         -> 'T'
  LFOCompareStoredHash -> 'H'
  LFOCopy              -> 'C'
  LFOCompareHashes     -> 'H'

data Pool
  = PoolCPU
  | PoolIO
    deriving (Eq, Show, Generic, Ord)

poolLabel :: Pool -> String
poolLabel = \case
  PoolCPU -> "CPU"
  PoolIO  -> "IO "

-- TODO explicit CPU/IO ordering please
showLibraryMapStatusPerJob :: LibraryMapStatus -> String
showLibraryMapStatusPerJob s =
    unlines poolWorkerListDisplay <> "\n" <> unlines poolJobCounts
  where
    poolWorkerListDisplay =
        pools s
            & Map.toList
            & map (\(p, wps) -> displayWorkers p (workers wps))
            & concat
    displayWorkers p wsm = Map.toList wsm & map (displayWorker p)
    displayWorker p (workerId, ws) = List.intercalate " | " $
        poolLabel p <> " " <> show workerId : displayWorkerPart ws
    displayWorkerPart = \case
      WorkerIdle -> ["[nothing to do]"]
      WorkerBusy fp op -> [fp, lfoVerbing op]
    poolJobCounts = map poolJobCount $ Map.toList $ pools s
    poolJobCount (p, ps) = poolLabel p <> " " <> displayCounter ps
    displayCounter ps =
        show (wpsProcessedJobs ps)
        <> "/"
        <> if lmsFullyTraversed s then show (scheduledJobs ps) else "?"

--------------------------------------------------------------------------------

data Msg
  = MsgFullyTraversed
  | MsgPoolUpdate Pool PoolUpdate
    deriving (Eq, Show)

data PoolUpdate
  = PoolUpdateQueued
  | PoolUpdateWorker Int WorkerStatus
  | PoolUpdateClosed
    deriving (Eq, Show)

processMsg :: LibraryMapStatus -> Msg -> LibraryMapStatus
processMsg lms = \case
  MsgFullyTraversed  -> lms { lmsFullyTraversed = True }
  MsgPoolUpdate p pu ->
    case pu of
      PoolUpdateClosed -> updatePoolInStatus p $ set (the @"closed") True
      PoolUpdateQueued -> updatePoolInStatus p $ over (the @"scheduledJobs") (+1)
      PoolUpdateWorker workerId ws ->
        updatePoolInStatus p $ over (the @"workers") $ Map.insert workerId ws
  where
    updatePoolInStatus p f = over (the @"pools") (tryUpdatePoolInner p f) lms
    tryUpdatePoolInner p f = Map.insertWith (\_ p' -> f p') p (f poolStatusDef)
    poolStatusDef = WorkerPoolStatus 0 0 False Map.empty

statusIndicatesFinished :: LibraryMapStatus -> Bool
statusIndicatesFinished lms = Map.foldr (\a b -> b && closed a) True (pools lms)

--------------------------------------------------------------------------------

libMapStatusExample :: LibraryMapStatus
libMapStatusExample = LibraryMapStatus
  { lmsFullyTraversed = True
  , pools = Map.fromList
      [ (PoolIO,  WorkerPoolStatus 50 35 False ioWorkers)
      , (PoolCPU, WorkerPoolStatus 3  2  False cpuWorkers) ]
  } where
    ioWorkers = Map.fromList
      [ (1, WorkerBusy "vocaloid/gizen-seigi.mp3"         LFOCompareHashes)
      , (2, WorkerBusy "bill-wurtz/more-than-a-dream.mp3" LFOCopy) ]
    cpuWorkers = Map.fromList
      [ (1, WorkerBusy "toby-fox/banger.flac" LFOTranscode)
      , (2, WorkerIdle) ]
