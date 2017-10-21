module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier(..)
  ,TransitionInfo(..)
  ,UtcTransitionsMap
  ,LeapsMap
  ,IntervalEntry(..)
  ,CalDateTransitionsMap
  ,emptyUtcTransitions
  ,addUtcTransition
  ,activeTransitionFor
  ,nextTransition
  ,emptyLeapsMap
  ,importLeaps
  ,addLeapTransition
  ,mergeLeapMaps
  ,activeLeapsFor
  ,emptyCalDateTransitions
  ,addCalDateTransition
  ,calDateTransitionsFor
  ,TimeZone(..)
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
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

activeTransitionFor :: Instant -> UtcTransitionsMap -> TransitionInfo
activeTransitionFor i utcM = snd . fromMaybe (Map.findMin utcM) $ Map.lookupLE i utcM     -- TODO: The findMin case should be impossible actually

nextTransition :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
nextTransition t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

-- Leap seconds

type LeapsMap = Map Instant Int

emptyLeapsMap :: LeapsMap
emptyLeapsMap = Map.empty

importLeaps :: [(Instant, Int)] -> LeapsMap
importLeaps = Map.fromList

addLeapTransition :: Instant -> Int -> LeapsMap -> LeapsMap
addLeapTransition = Map.insert

mergeLeapMaps :: LeapsMap -> LeapsMap -> LeapsMap
mergeLeapMaps = Map.union

activeLeapsFor :: Instant -> LeapsMap -> Int
activeLeapsFor i leapsM = fromMaybe 0 $ fmap snd $ Map.lookupLE i leapsM

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

calDateTransitionsFor :: Instant -> CalDateTransitionsMap -> [TransitionInfo]
calDateTransitionsFor i = fmap snd . IMap.search (Entry i)

-- | Represents a time zone.  A 'TimeZone' can be used to instanciate a 'ZoneDateTime' from either and 'Instant' or a 'CalendarDateTime'
data TimeZone =
  TimeZone
    {
       zoneName :: TZIdentifier
      ,utcTransitionsMap :: UtcTransitionsMap
      ,calDateTransitionsMap :: CalDateTransitionsMap
      ,leapsMap :: LeapsMap
    }
  deriving (Eq, Show)
