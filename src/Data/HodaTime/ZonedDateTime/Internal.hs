module Data.HodaTime.ZonedDateTime.Internal
(
   ZonedDateTime(..)
  ,ZoneLocalResult(..)
)
where

import Data.HodaTime.OffsetDateTime.Internal (OffsetDateTime)
import Data.HodaTime.TimeZone.Internal (TimeZone)

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }     -- TODO: It's not yet clear that we would need an offset time here

data ZoneLocalResult =
    ZLSingle ZonedDateTime
  | ZLMulti { zlmFirst :: ZonedDateTime, zlmLast :: ZonedDateTime }
