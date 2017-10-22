-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Instant
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'ZonedDateTime'.  A 'ZonedDateTime' is a universal time, it represents the same moment anywhere in the world and is completely
-- unambiguous.  Each instance of a 'ZonedDateTime' corresponds to exactly one point on a continuous time line.
----------------------------------------------------------------------------
module Data.HodaTime.ZonedDateTime
(
  -- * Types
   ZonedDateTime
  -- * Constructors
  ,fromCalendarDateTimeStrictly
  ,fromCalendarDateTimeAll
  -- * Math
  -- * Conversion
  ,toLocalDateTime
  ,toLocalDate
  ,toLocalTime
)
where

import Data.HodaTime.ZonedDateTime.Internal
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime(..), CalendarDate(..), IsCalendarDateTime(..), LocalTime)
import Data.HodaTime.TimeZone.Internal (TimeZone(..), calDateTransitionsFor)

-- constructors

fromCalendarDateTimeStrictly :: IsCalendarDateTime cal => CalendarDateTime cal -> TimeZone -> Maybe (ZonedDateTime cal)
fromCalendarDateTimeStrictly cdt = go . fromCalendarDateTimeAll cdt
  where
    go [zdt] = Just zdt
    go _ = Nothing

-- | Return all 'ZonedDateTime' entries for a specific 'CalendarDateTime' in a 'TimeZone'. Normally this would be one, but in the case that a time
-- occurs twice in a zone (i.e. due to daylight savings time change) both would be returned.  Also, if the time does not occur at all, an empty list
-- will be returned.  This method allows the user to choose exactly what to do in the case of ambigiuty.
fromCalendarDateTimeAll :: IsCalendarDateTime cal => CalendarDateTime cal -> TimeZone -> [ZonedDateTime cal]
fromCalendarDateTimeAll cdt tz@(TimeZone _ _ calDateM _) = zdts
  where
    instant = toUnadjustedInstant cdt
    zdts = fmap mkZdt . calDateTransitionsFor instant $ calDateM
    mkZdt = ZonedDateTime cdt tz

-- conversion

-- | Return the 'CalendarDateTime' represented by this 'ZonedDateTime'.
toLocalDateTime :: ZonedDateTime cal -> CalendarDateTime cal
toLocalDateTime (ZonedDateTime cdt _  _) = cdt

-- | Return the 'CalendarDate' represented by this 'ZonedDateTime'.
toLocalDate :: ZonedDateTime cal -> CalendarDate cal
toLocalDate (ZonedDateTime (CalendarDateTime cd _) _  _) = cd

-- | Return the 'LocalTime' represented by this 'ZonedDateTime'.
toLocalTime :: ZonedDateTime cal -> LocalTime
toLocalTime (ZonedDateTime (CalendarDateTime _ lt) _  _) = lt