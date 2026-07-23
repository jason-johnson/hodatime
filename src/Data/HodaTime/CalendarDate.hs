-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.CalendarDate
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate'.  A 'CalendarDate' represents a date within the calendar system that is part of its type.  It has no reference to a particular time zone or time of day.
--
-- === Construction
--
-- To construct one of these types, see the Calendar module you wish to construct the date in (typically "Data.HodaTime.Calendar.Gregorian")
----------------------------------------------------------------------------
module Data.HodaTime.CalendarDate
(
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate
  ,HasDate(..)
  ,withCalendar
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDate, DayNth(..), DayOfMonth, Year, WeekNumber, HasDate(..), CalendarDateTime(..), IsCalendarDateTime(..), at)
import Data.HodaTime.LocalTime.Internal (midnight)

-- | Re-express a 'CalendarDate' in a different calendar, preserving the same day on the absolute timeline.  For
--   example, convert a Gregorian date to the Julian (\"Old Calendar\") date still used liturgically by the Eastern
--   Orthodox church.  The target calendar is chosen by the result type (via @TypeApplications@ or a type annotation).
withCalendar :: (IsCalendarDateTime a, IsCalendarDateTime b) => CalendarDate a -> CalendarDate b
withCalendar date = date'
  where CalendarDateTime date' _ = fromAdjustedInstant (toUnadjustedInstant (date `at` midnight))
