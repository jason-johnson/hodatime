module Data.HodaTime.ZonedDateTime.Internal
(
  ZonedDateTime(..)
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime)
import Data.HodaTime.TimeZone.Internal (TimeZone)

-- | A CalendarDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime cal = ZonedDateTime { zdtCalendarDateTime :: CalendarDateTime cal, zdtTimeZone :: TimeZone }
  deriving (Eq, Show)
-- TODO: We should have an Ord instance, we can just ignore the timezone field

-- helper functions

-- TODO: We need functions that help construct this type.  Some of those functions probably need to be in OffsetDateTime so we can hide details of
-- TODO: CalendarDateTime from this module.  What we're trying to do is make sure the OffsetDateTime has the time set to the local time zone
-- TODO: and that the offset part tells us how far we are from UTC
