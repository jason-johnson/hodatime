module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier
  ,TransitionInfo(..)
  ,UtcTransitionsMap
  ,LeapsMap
  ,emptyTransitions
  ,addTransition
  ,activeTransitionFor
  ,nextTransition
  ,importLeaps
  ,TimeZone(..)
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo { utcOffset :: Int, isDst :: Bool, abbreviation :: String }
  deriving (Eq, Show)

-- UTC instant to transition

type UtcTransitionsMap = Map Instant TransitionInfo

emptyTransitions :: UtcTransitionsMap
emptyTransitions = Map.empty

addTransition :: Instant -> TransitionInfo -> UtcTransitionsMap -> UtcTransitionsMap
addTransition = Map.insert

activeTransitionFor :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
activeTransitionFor t ts = fromMaybe (Map.findMin ts) $ Map.lookupLE t ts

nextTransition :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
nextTransition t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

-- Leap seconds

type LeapsMap = Map Instant Int

importLeaps :: [(Instant, Int)] -> LeapsMap
importLeaps = Map.fromList

-- TODO: Right now we have the mapping from UTC instance to an offset, but we need an interval set for mapping from local time to the offset.
-- TODO: IMPORTANT: the utcTransitionMap must have the key in UTC because it will be coming from UTC instances.  The interval map *must be*
-- TODO: in localtime because it will be coming from localtime, not UTC
data TimeZone = TimeZone { zone :: TZIdentifier, utcTransitionsMap :: UtcTransitionsMap }
  deriving (Eq, Show)
