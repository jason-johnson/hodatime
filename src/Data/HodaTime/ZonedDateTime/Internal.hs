module Data.HodaTime.ZonedDateTime.Internal
(
   ZonedDateTime(..)
  ,fromInstant
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime, fromAdjustedInstant)
import Data.HodaTime.TimeZone.Internal (TimeZone, TransitionInfo, activeTransitionFor, tiUtcOffset)
import Data.HodaTime.Offset.Internal (adjustInstant)
import Data.HodaTime.Instant.Internal (Instant)

-- | A CalendarDateTime in a specific time zone. A 'ZonedDateTime' is global and maps directly to a single 'Instant'.
data ZonedDateTime cal = ZonedDateTime { zdtCalendarDateTime :: CalendarDateTime cal, zdtTimeZone :: TimeZone, zdtActiveTransition :: TransitionInfo }
  deriving (Eq, Show)
-- TODO: We should have an Ord instance, we can just ignore the timezone field.  It would be especially good so that when CalendarDateTime is equal we can
-- TODO: compare the TransitionInfo to see which one comes first


-- | Returns the 'ZonedDateTime' represented by the passed 'Instant' within the given 'TimeZone'.  This is always an unambiguous conversion.
fromInstant :: IsCalendarDateTime cal => Instant -> TimeZone -> ZonedDateTime cal
fromInstant instant tz = ZonedDateTime cdt tz ti
  where
    ti = activeTransitionFor instant tz
    offset = tiUtcOffset ti
    instant' = adjustInstant offset instant
    cdt = fromAdjustedInstant instant'

-- helper functions

-- TODO: We need functions that help construct this type.  Some of those functions probably need to be in OffsetDateTime so we can hide details of
-- TODO: CalendarDateTime from this module.  What we're trying to do is make sure the OffsetDateTime has the time set to the local time zone
-- TODO: and that the offset part tells us how far we are from UTC.  Nanos tell us how far we are into the current day
