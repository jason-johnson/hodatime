module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier
  ,TransitionInfo(..)
  ,Transitions
  ,mkTransitions
  ,addTransitionInfo
  ,activeTransitionInfoFor
  ,nextTransitionInfo
  ,TimeZone(..)
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype TZIdentifier = Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo {
   ttGmtOffset :: Int
  ,ttIsDst :: Bool
  ,ttAbbreviation :: String
  ,ttIsStd :: Bool
  ,ttIsGmt :: Bool } deriving (Eq, Show)

type Transitions = Map Instant TransitionInfo

mkTransitions :: Transitions
mkTransitions = Map.empty

addTransitionInfo :: Instant -> TransitionInfo -> Transitions -> Transitions
addTransitionInfo = Map.insert

activeTransitionInfoFor :: Instant -> Transitions -> (Instant, TransitionInfo)
activeTransitionInfoFor t ts = fromMaybe (Map.findMin ts) $ Map.lookupLE t ts

nextTransitionInfo :: Instant -> Transitions -> (Instant, TransitionInfo)
nextTransitionInfo t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

data TimeZone =
    UTCzone
  | TimeZone {
     tzZone :: TZIdentifier
    ,tzTransitions :: Transitions }
      deriving (Eq, Show)
