module Data.HodaTime.TimeZone
(
   utc
  ,localZone
  ,timeZone
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime, ZoneLocalResult(..))
import Data.HodaTime.OffsetDateTime.Internal (Offset)

-- TODO: Leap seconds will make this an IO like the others
utc :: TimeZone
utc = TimeZone UTC emptyUtcTransitions emptyCalDateTransitions

timeZone :: String -> Maybe TimeZone
timeZone = undefined

localZone :: TimeZone
localZone = undefined
