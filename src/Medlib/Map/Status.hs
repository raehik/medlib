{-
Functions over the library map operation status: mutating, printing, checking.

To allow clean thread programming, all operation state is thrust towards a
single thread that tracks it in a single place, and can handle displaying
various information. It also means we don't get much safety from type
guarantees, so we have to model the system with care.

In particular, pool closure isn't maybe not very safe. But it seems a smart
approach?
-}

{-# LANGUAGE DataKinds #-}

module Medlib.Map.Status where

import           Medlib.Map

import           GHC.Generics
import           Data.Function
import qualified Data.Map       as Map
import           Data.Map       ( Map )
import qualified Data.List      as List
import           Optics
import           Data.Generics.Product.Any

statusDef :: Status
statusDef = Status { fullyTraversed = False, pools = Map.empty }

opVerb :: Op -> String
opVerb = \case
  OpTranscode         -> "transcode          "
  OpCompareStoredHash -> "compare stored hash"
  OpCompareStoredSize -> "compare stored size"
  OpCopy              -> "copy               "
  OpCompareHashes     -> "compare hashes     "
  OpCompareSizes      -> "compare sizes      "

opID :: Op -> Char
opID = \case
  OpTranscode         -> 'T'
  OpCompareStoredHash -> 'H'
  OpCompareStoredSize -> 'S'
  OpCopy              -> 'C'
  OpCompareHashes     -> 'H'
  OpCompareSizes      -> 'S'

poolLabel :: ResourceBound -> String
poolLabel = \case
  ResourceBoundCPU -> "CPU"
  ResourceBoundIO  -> "IO "

-- TODO explicit CPU/IO ordering please
showStatusEachSlot :: Status -> String
showStatusEachSlot s = List.intercalate "\n" poolSlotListDisplay
                       <> "\n\n"
                       <> List.intercalate "\n" poolJobCounts
  where
    poolSlotListDisplay =
        pools s
            & Map.toList
            & map (\(p, ps) -> displaySlots p (slots ps))
            & concat
    displaySlots p ssm = Map.toList ssm & map (displaySlot p)
    displaySlot p (slotId, ss) =
        let lhs = poolLabel p <> " " <> show slotId
         in List.intercalate " | " (lhs : displaySlotPart ss)
    displaySlotPart = \case
      SlotIdle -> ["<idle>"]
      SlotBusy fp op -> [opVerb op, take 100 fp] -- TODO improve!
    poolJobCounts = map poolJobCount $ Map.toList $ pools s
    poolJobCount (p, ps) = poolLabel p <> " " <> displayCounter ps
    displayCounter ps =
        show (completedJobs ps)
        <> "/"
        <> if fullyTraversed s then show (scheduledJobs ps) else "?"

--------------------------------------------------------------------------------

-- TODO I'm cheating and using SlotIdle to indicate a complete job. If that gets
-- awkward, I should add another slot status - not a kludge, would be useful!
processUpdate :: Status -> Update -> Status
processUpdate s = \case
  UpdateFullyTraversed  -> set (the @"fullyTraversed") True s
  UpdatePool p pu ->
    case pu of
      PoolUpdateClosed -> updatePoolInStatus p $ set (the @"closed") True
      PoolUpdateQueued -> updatePoolInStatus p $ over (the @"scheduledJobs") (+1)
      PoolUpdateSlot slotId ss ->
        let s' = case ss of
                   SlotIdle -> -- slot gone idle => job complete!
                     updatePoolInStatus p $ over (the @"completedJobs") (+1)
                   _ -> s
         in updatePoolInStatus' s' p $ over (the @"slots") $ Map.insert slotId ss
  where
    updatePoolInStatus = updatePoolInStatus' s
    updatePoolInStatus' s' p f = over (the @"pools") (tryUpdatePoolInner p f) s'
    tryUpdatePoolInner p f = Map.insertWith (\_ p' -> f p') p (f poolStatusDef)
    poolStatusDef = PoolStatus 0 0 False Map.empty

-- We check for pools reporting finished, and fully traversed.
-- TODO inefficient?
indicatesFinished :: Status -> Bool
indicatesFinished s = Map.foldr (\a b -> b && closed a) (fullyTraversed s) (pools s)

--------------------------------------------------------------------------------

exampleStatus :: Status
exampleStatus = Status
  { fullyTraversed = True
  , pools = Map.fromList
      [ (ResourceBoundIO,  PoolStatus 50 35 False ioSlots)
      , (ResourceBoundCPU, PoolStatus 3  2  False cpuSlots) ]
  } where
    ioSlots = Map.fromList
      [ (1, SlotBusy "vocaloid/gizen-seigi.mp3"         OpCompareHashes)
      , (2, SlotBusy "bill-wurtz/more-than-a-dream.mp3" OpCopy) ]
    cpuSlots = Map.fromList
      [ (1, SlotBusy "toby-fox/banger.flac" OpTranscode)
      , (2, SlotIdle) ]
