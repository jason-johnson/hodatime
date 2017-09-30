module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier
  ,Transition(..)
  ,Transitions
  ,mkTransitions
  ,addTransition
  ,activeTransitionFor
  ,nextTransition
  ,TimeZone(..)
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data Transition = TransitionInfo { tOffset :: Int, tIsDst :: Bool, tAbbreviation :: String, tIsStd :: Bool, tIsGmt :: Bool }
  deriving (Eq, Show)

type Transitions = Map Instant Transition

mkTransitions :: Transitions
mkTransitions = Map.empty

addTransition :: Instant -> Transition -> Transitions -> Transitions
addTransition = Map.insert

activeTransitionFor :: Instant -> Transitions -> (Instant, Transition)
activeTransitionFor t ts = fromMaybe (Map.findMin ts) $ Map.lookupLE t ts

nextTransition :: Instant -> Transitions -> (Instant, Transition)
nextTransition t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

data TimeZone = TimeZone { tzZone :: TZIdentifier, tzTransitions :: Transitions }
  deriving (Eq, Show)
