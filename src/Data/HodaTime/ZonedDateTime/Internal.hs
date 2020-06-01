module Data.HodaTime.ZonedDateTime.Internal
(
   ZonedDateTime(..)
  ,fromInstant
  ,year
  ,month
  ,day
  ,hour
  ,minute
  ,second
  ,nanosecond
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime, IsCalendar, fromAdjustedInstant)
import qualified Data.HodaTime.CalendarDateTime.Internal as CDT
import qualified Data.HodaTime.LocalTime.Internal as LT
import Data.HodaTime.TimeZone.Internal (TimeZone, TransitionInfo, activeTransitionFor, tiUtcOffset)
import Data.HodaTime.Offset.Internal (adjustInstant)
import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.Internal.Lens (view)

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

-- | Accessor for the Year of a 'ZonedDateTime'.
year :: IsCalendar cal => ZonedDateTime cal -> CDT.Year
year (ZonedDateTime cdt _ _) = view CDT.year cdt

-- | Accessor for the Month of a 'ZonedDateTime'.
month :: IsCalendar cal => ZonedDateTime cal -> CDT.Month cal
month (ZonedDateTime cdt _ _) = CDT.month cdt

-- | Accessor for the Day of a 'ZonedDateTime'.
day :: IsCalendar cal => ZonedDateTime cal -> CDT.DayOfMonth
day (ZonedDateTime cdt _ _) = view CDT.day cdt

-- | Accessor for the Hour of a 'ZonedDateTime'.
hour :: IsCalendar cal => ZonedDateTime cal -> LT.Hour
hour (ZonedDateTime cdt _ _) = view LT.hour cdt

-- | Accessor for the Minute of a 'ZonedDateTime'.
minute :: IsCalendar cal => ZonedDateTime cal -> LT.Minute
minute (ZonedDateTime cdt _ _) = view LT.minute cdt

-- | Accessor for the Second of a 'ZonedDateTime'.
second :: IsCalendar cal => ZonedDateTime cal -> LT.Second
second (ZonedDateTime cdt _ _) = view LT.second cdt

-- | Accessor for the Nanosecond of a 'ZonedDateTime'.
nanosecond :: IsCalendar cal => ZonedDateTime cal -> LT.Nanosecond
nanosecond (ZonedDateTime cdt _ _) = view LT.nanosecond cdt

-- helper functions

-- TODO: We need functions that help construct this type.  Some of those functions probably need to be in OffsetDateTime so we can hide details of
-- TODO: CalendarDateTime from this module.  What we're trying to do is make sure the OffsetDateTime has the time set to the local time zone
-- TODO: and that the offset part tells us how far we are from UTC.  Nanos tell us how far we are into the current day
