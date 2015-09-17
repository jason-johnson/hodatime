module Data.HodaTime.ZonedDateTime.Internal
(
   TimeZoneName(..)
  ,TransitionType(..)
  ,Transition(..)
  ,TimeZone(..)
  ,ZonedDateTime(..)
)
where

import Data.HodaTime.OffsetDateTime.Internal(OffsetDateTime)

type TZIdentifier = String

data TimeZoneName =
      UTC
    | Zone TZIdentifier
    deriving(Eq, Show)

data TransitionType = TransitionType {
     ttGmtOffset :: Int
    ,ttIsDst :: Bool
    ,ttAbbreviation :: String
    ,ttIsStd :: Bool
    ,ttIsGmt :: Bool } deriving (Eq, Show)

data Transition = Transition {
     tOffset :: Int
    ,tType :: TransitionType } deriving (Eq, Show)

data TimeZone =
      UTCzone
    | TimeZone {
         tzZone :: TZIdentifier
        ,tzTransitions :: [Transition] } deriving (Eq, Show)

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }     -- TODO: It's not yet clear that we would need an offset time here