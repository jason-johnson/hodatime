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
--
-- === Cookbook
--
-- ==== Building and inspecting a date
--
-- Dates are built with a calendar's smart constructor, which returns 'Nothing' for a date that does not exist.
-- Read-only components (such as the day of the week) are plain functions.
--
-- >>> calendarDate 14 February 2024
-- Just (fromJust (Gregorian.calendarDate 14 February 2024))
-- >>> calendarDate 30 February 2024
-- Nothing
-- >>> dayOfWeek <$> calendarDate 14 February 2024
-- Just Wednesday
--
-- ==== Re-expressing a date in another calendar
--
-- 'withCalendar' re-expresses a date in another calendar, preserving the same day on the absolute timeline; the
-- target calendar is chosen by the result type.  Here the Gregorian Christmas becomes the Julian (\"Old Calendar\")
-- date still used liturgically, which the Julian calendar labels 12 December 2024 (it runs thirteen days behind).
--
-- >>> withCalendar <$> calendarDate 25 December 2024 :: Maybe (CalendarDate Julian.Julian)
-- Just (fromJust (Julian.calendarDate 12 December 2024))
--
-- ==== USA holidays for a given year
--
-- > import Data.Maybe (catMaybes)
-- > import Data.HodaTime.CalendarDate (DayNth(..))
-- > import Data.HodaTime.Calendar.Gregorian (calendarDate, fromNthDay, Month(..), DayOfWeek(..))
-- >
-- > usaHolidays y = catMaybes $ (\$ y) \<$\>
-- >   [
-- >      calendarDate 1 January               -- New Year
-- >     ,calendarDate 4 July                  -- Independence Day
-- >     ,calendarDate 25 December             -- Christmas
-- >     ,fromNthDay First Monday September    -- Labor day
-- >     ,fromNthDay Third Monday January      -- MLK day
-- >     ,fromNthDay Second Tuesday February   -- Presidents day
-- >     ,fromNthDay Fourth Thursday November  -- Thanksgiving
-- >     ,calendarDate 29 February             -- Leap day (not a real holiday, but shows a date that may not exist)
-- >   ]
----------------------------------------------------------------------------
module Data.HodaTime.CalendarDate
(
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate
  ,HasDate
  ,DoW
  ,MoY
  ,day
  ,month
  ,year
  ,dayOfWeek
  ,next
  ,previous
  ,yearMonthDay
  ,withCalendar
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDate, DayNth(..), DayOfMonth, Year, WeekNumber, HasDate, DoW, MoY, day, month, year, dayOfWeek, next, previous, yearMonthDay, CalendarDateTime(..), IsCalendarDateTime(..), at)
import Data.HodaTime.LocalTime.Internal (midnight)

-- $setup
-- >>> import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..))
-- >>> import qualified Data.HodaTime.Calendar.Julian as Julian

-- | Re-express a 'CalendarDate' in a different calendar, preserving the same day on the absolute timeline.  For
--   example, convert a Gregorian date to the Julian (\"Old Calendar\") date still used liturgically by the Eastern
--   Orthodox church.  The target calendar is chosen by the result type (via @TypeApplications@ or a type annotation).
withCalendar :: (IsCalendarDateTime a, IsCalendarDateTime b) => CalendarDate a -> CalendarDate b
withCalendar date = date'
  where CalendarDateTime date' _ = fromAdjustedInstant (toUnadjustedInstant (date `at` midnight))
