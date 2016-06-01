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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

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

type Transitions = IntMap TransitionInfo

mkTransitions :: Transitions
mkTransitions = IM.empty

addTransitionInfo :: TransitionInfo -> Int -> Transitions -> Transitions
addTransitionInfo ti t = IM.insert t ti

transitionInfoAt :: Int -> Transitions -> (Int, TransitionInfo)
transitionInfoAt t ts = fromMaybe (IM.findMin ts) $ IM.lookupLE t ts

data TimeZone =
      UTCzone
    | TimeZone {
         tzZone :: TZIdentifier
        ,tzTransitions :: Transitions } deriving (Eq, Show)

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }     -- TODO: It's not yet clear that we would need an offset time here
