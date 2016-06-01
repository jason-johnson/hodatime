module Data.HodaTime.ZonedDateTime.Internal
(
   TimeZoneName(..)
  ,TransitionInfo(..)
  ,Transitions
  ,mkTransitions
  ,addTransitionInfo
  ,transitionInfoAt
  ,TimeZone(..)
  ,ZonedDateTime(..)
)
where

import Data.HodaTime.OffsetDateTime.Internal (OffsetDateTime)
import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type TZIdentifier = String

data TimeZoneName =
      UTC
    | Local
    | Zone TZIdentifier
    deriving(Eq, Show)

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

data TimeZone =
      UTCzone
    | TimeZone {
         tzZone :: TZIdentifier
        ,tzTransitions :: Transitions } deriving (Eq, Show)

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }     -- TODO: It's not yet clear that we would need an offset time here
