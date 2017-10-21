module Data.HodaTime.ZonedDateTime
(
  fromCalendarDateTimeAll
)
where

import Data.HodaTime.ZonedDateTime.Internal
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime(..), IsCalendarDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..), calDateTransitionsFor)

-- | Return all 'ZonedDateTime' entries for a specific 'CalendarDateTime' in a 'TimeZone'. Normally this would be one, but in the case that a time
-- occurs twice in a zone (i.e. due to daylight savings time change) both would be returned.  Also, if the time does not occur at all, an empty list
-- will be returned.  This method allows the user to choose exactly what to do in the case of ambigiuty.
fromCalendarDateTimeAll :: IsCalendarDateTime cal => CalendarDateTime cal -> TimeZone -> [ZonedDateTime cal]
fromCalendarDateTimeAll cdt tz@(TimeZone _ _ calDateM _) = zdts
  where
    instant = toUnadjustedInstant cdt
    zdts = fmap mkZdt . calDateTransitionsFor instant $ calDateM
    mkZdt = ZonedDateTime cdt tz