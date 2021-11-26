{- |
Library map operation data types.

Note that though we track workers by indexing them with a number, the implementation 
-}

module Medlib.Map
  (
  -- * Core types
    ResourceBound(..)

  -- * Status types
  , Status(..)
  , PoolStatus(..)
  , SlotStatus(..)
  , Op(..)

  -- * Status updates
  , Update(..)
  , PoolUpdate(..)
  ) where

import           GHC.Generics
import           Data.Function
import qualified Data.Map       as Map
import           Data.Map       ( Map )
import qualified Data.List      as List
import           Optics
import           Data.Generics.Product.Any

data ResourceBound
  = ResourceBoundCPU
  | ResourceBoundIO
    deriving (Eq, Show, Generic, Ord)

--------------------------------------------------------------------------------

-- | Status of a running library map operation.
data Status = Status
  { fullyTraversed :: Bool
  -- ^ Is the source library fully traversed? (This is useful for status
  --   displays to know, since it means pools won't have any more jobs
  --   scheduled so a "remaining jobs" tracker becomes useful.)
  , pools          :: Map ResourceBound PoolStatus
  } deriving (Eq, Show, Generic)

data PoolStatus = PoolStatus
  { scheduledJobs :: Int
  -- ^ The total number of jobs sent to the pool. Note that this might not be
  --   interesting until we know the pool shall not receive any further jobs.
  , completedJobs :: Int
  , closed        :: Bool
  -- ^ 'True' means is the pool no longer accepting jobs, and no more slots will
  --   be allocated. TODO: actually no, this means the pool is finished and all
  --   slots are done. I think this is useless.
  , slots         :: Map Int SlotStatus
  } deriving (Eq, Show, Generic, Ord)

data SlotStatus
  = SlotBusy FilePath Op
  | SlotIdle
    deriving (Eq, Show, Generic, Ord)

data Op
  = OpTranscode
  | OpCompareStoredHash
  | OpCopy
  | OpCompareHashes
    deriving (Eq, Show, Generic, Ord)

--------------------------------------------------------------------------------

data Update
  = UpdateFullyTraversed
  | UpdatePool ResourceBound PoolUpdate
    deriving (Eq, Show)

data PoolUpdate
  = PoolUpdateQueued
  | PoolUpdateSlot Int SlotStatus
  | PoolUpdateClosed
    deriving (Eq, Show)
