-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.OffsetDateTime
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'OffsetDateTime' is a date and time combined with an offset from UTC time.  'OffsetDateTime' is the form that HTTP uses to deal with dates and times.
----------------------------------------------------------------------------
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

import Data.HodaTime.Offset.Internal
import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..), TZIdentifier(..), TransitionInfo, fixedOffsetZone)

-- | A 'CalendarDateTime' with a UTC offset.  This is the format used by e.g. HTTP.  This type has a fixed 'TimeZone' with the name "UTC(+/-)offset".  If the offset is
-- empty, the name of the 'TimeZone' will be UTC
newtype OffsetDateTime cal = OffsetDateTime (ZonedDateTime cal)
  deriving (Eq, Show)    -- TODO: Remove Show

-- | Create an 'OffsetDateTime' from an 'Instant' and an 'Offset'.
fromInstantWithOffset :: IsCalendarDateTime cal => Instant -> Offset -> OffsetDateTime cal
fromInstantWithOffset inst offset = OffsetDateTime $ ZonedDateTime cdt tz tInfo
  where
    (tz, tInfo) = makeFixedTimeZone offset
    cdt = fromAdjustedInstant . adjustInstant offset $ inst

-- | Create an 'OffsetDateTime' from a 'CalendarDateTime' and an 'Offset'.
fromCalendarDateTimeWithOffset :: CalendarDateTime cal -> Offset -> OffsetDateTime cal
fromCalendarDateTimeWithOffset cdt offset = OffsetDateTime $ ZonedDateTime cdt tz tInfo
  where
    (tz, tInfo) = makeFixedTimeZone offset

-- helper functions

makeFixedTimeZone :: Offset -> (TimeZone, TransitionInfo)
makeFixedTimeZone offset = (TimeZone (Zone tzName) utcM calDateM, tInfo)
  where
    tzName = toStringRep offset
    (utcM, calDateM, tInfo) = fixedOffsetZone tzName offset