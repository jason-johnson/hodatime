module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier
  ,TransitionInfo(..)
  ,Transitions
  ,mkTransitions
  ,addTransitionInfo
  ,transitionInfoAt
  ,transitionInfoAfter
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

transitionInfoAt :: Instant -> Transitions -> (Instant, TransitionInfo)
transitionInfoAt t ts = fromMaybe (Map.findMin ts) $ Map.lookupLE t ts

transitionInfoAfter :: Instant -> Transitions -> (Instant, TransitionInfo)
transitionInfoAfter t ts = fromMaybe (Map.findMax ts) $ Map.lookupGT t ts

data TimeZone =
    UTCzone
  | TimeZone {
     tzZone :: TZIdentifier
    ,tzTransitions :: Transitions } deriving (Eq, Show)
