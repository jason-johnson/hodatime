module Data.HodaTime.ZonedDateTime.Internal
(
   TimeZone(..)
  ,ZonedDateTime(..)
)
where

import Data.HodaTime.OffsetDateTime.Internal(OffsetDateTime)

data TimeZone = TimeZone { }

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }