module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier
  ,TransitionInfo(..)
  ,UtcTransitionsMap
  ,LeapsMap
  ,IntervalEntry(..)
  ,CalDateTransitionsMap
  ,emptyUtcTransitions
  ,addUtcTransition
  ,activeTransitionFor
  ,nextTransition
  ,importLeaps
  ,emptyCalDateTransitions
  ,addCalDateTransition
  ,TimeZone(..)
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IMap

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo { utcOffset :: Int, isDst :: Bool, abbreviation :: String }
  deriving (Eq, Show)

-- UTC instant to transition

type UtcTransitionsMap = Map Instant TransitionInfo

emptyUtcTransitions :: UtcTransitionsMap
emptyUtcTransitions = Map.empty

addUtcTransition :: Instant -> TransitionInfo -> UtcTransitionsMap -> UtcTransitionsMap
addUtcTransition = Map.insert

activeTransitionFor :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
activeTransitionFor t ts = fromMaybe (Map.findMin ts) $ Map.lookupLE t ts

nextTransition :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
nextTransition t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

-- Leap seconds

type LeapsMap = Map Instant Int

importLeaps :: [(Instant, Int)] -> LeapsMap
importLeaps = Map.fromList

-- CalendarDate to transition

data IntervalEntry a =
    Smallest
  | Entry a
  | Largest
  deriving (Eq, Ord, Show)

type CalDateTransitionsMap = IntervalMap (IntervalEntry Instant) TransitionInfo

emptyCalDateTransitions :: CalDateTransitionsMap
emptyCalDateTransitions = IMap.empty

addCalDateTransition :: IntervalEntry Instant -> IntervalEntry Instant -> TransitionInfo -> CalDateTransitionsMap -> CalDateTransitionsMap
addCalDateTransition b e = IMap.insert interval
  where
    interval = Interval b e

-- TODO: Right now we have the mapping from UTC instance to an offset, but we need an interval set for mapping from local time to the offset.
-- TODO: IMPORTANT: the utcTransitionMap must have the key in UTC because it will be coming from UTC instances.  The interval map *must be*
-- TODO: in localtime because it will be coming from localtime, not UTC
data TimeZone = TimeZone { zone :: TZIdentifier, utcTransitionsMap :: UtcTransitionsMap, calDateTransitionsMap :: CalDateTransitionsMap }
  deriving (Eq, Show)
