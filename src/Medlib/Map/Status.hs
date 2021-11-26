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

opVerbing :: Op -> String
opVerbing = \case
  OpTranscode         -> "transcoding"
  OpCompareStoredHash -> "checking transcoded stored hash"
  OpCopy              -> "copying"
  OpCompareHashes     -> "checking hashes"

opID :: Op -> Char
opID = \case
  OpTranscode         -> 'T'
  OpCompareStoredHash -> 'H'
  OpCopy              -> 'C'
  OpCompareHashes     -> 'H'

poolLabel :: ResourceBound -> String
poolLabel = \case
  ResourceBoundCPU -> "CPU"
  ResourceBoundIO -> "IO "

-- TODO explicit CPU/IO ordering please
showStatusEachSlot :: Status -> String
showStatusEachSlot s =
    unlines poolSlotListDisplay <> "\n" <> unlines poolJobCounts
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
      SlotIdle -> ["[nothing to do]"]
      SlotBusy fp op -> [fp, opVerbing op]
    poolJobCounts = map poolJobCount $ Map.toList $ pools s
    poolJobCount (p, ps) = poolLabel p <> " " <> displayCounter ps
    displayCounter ps =
        show (completedJobs ps)
        <> "/"
        <> if fullyTraversed s then show (scheduledJobs ps) else "?"

--------------------------------------------------------------------------------

processUpdate :: Status -> Update -> Status
processUpdate s = \case
  UpdateFullyTraversed  -> set (the @"fullyTraversed") True s
  UpdatePool p pu ->
    case pu of
      PoolUpdateClosed -> updatePoolInStatus p $ set (the @"closed") True
      PoolUpdateQueued -> updatePoolInStatus p $ over (the @"scheduledJobs") (+1)
      PoolUpdateSlot slotId ss ->
        updatePoolInStatus p $ over (the @"slots") $ Map.insert slotId ss
  where
    updatePoolInStatus p f = over (the @"pools") (tryUpdatePoolInner p f) s
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
