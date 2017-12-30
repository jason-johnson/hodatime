module Data.HodaTime.OffsetDateTime
(
  -- * Types
   OffsetDateTime
  -- * Constructors
  ,fromInstantWithOffset
  ,fromCalendarDateTimeWithOffset
  -- * Math
  -- * Conversion
)
where

import Data.HodaTime.OffsetDateTime.Internal
import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..), TZIdentifier(..), TransitionInfo)
import Data.HodaTime.TimeZone.Platform (fixedOffsetZone)

-- | Create an 'OffsetDateTime' from an 'Instant' and an 'Offset'.
fromInstantWithOffset :: IsCalendarDateTime cal => Instant -> Offset -> OffsetDateTime cal
fromInstantWithOffset inst offset = OffsetDateTime $ ZonedDateTime cdt tz tInfo
  where
    (tz, tInfo) = makeFixedTimeZone offset
    cdt = fromAdjustedInstant . adjustInstant secs $ inst
    secs = fromIntegral . offsetSeconds $ offset

-- | Create an 'OffsetDateTime' from a 'CalendarDateTime' and an 'Offset'.
fromCalendarDateTimeWithOffset :: CalendarDateTime cal -> Offset -> OffsetDateTime cal
fromCalendarDateTimeWithOffset cdt offset = OffsetDateTime $ ZonedDateTime cdt tz tInfo
  where
    (tz, tInfo) = makeFixedTimeZone offset

-- helper functions

makeFixedTimeZone :: Offset -> (TimeZone, TransitionInfo)
makeFixedTimeZone offset = (TimeZone (Zone tzName) utcM calDateM tExprDetails, tInfo)
  where
    tzName = toStringRep offset
    (utcM, calDateM, tExprDetails, tInfo) = fixedOffsetZone tzName (fromIntegral . offsetSeconds $ offset)