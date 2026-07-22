-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Gregorian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Gregorian' calendar, the civil calendar used across most of the world today.  The Gregorian calendar refines the Julian leap
-- year rule: every fourth year is a leap year, except that a century year (one divisible by 100) is a leap year only when it is also divisible by 400.  Those century exceptions keep the calendar
-- closely aligned to the solar year and correct the slow drift of 'Data.HodaTime.Calendar.Julian' (which gains roughly three days every four centuries).  Unlike some libraries this calendar is not
-- proleptic: because the Gregorian calendar first took effect on 15 October 1582 (the day after 4 October 1582 in the Julian calendar, when ten days were dropped) this implementation rejects any
-- earlier date \- use 'Data.HodaTime.Calendar.Julian' for dates before the changeover.  The Gregorian calendar anchors the absolute timeline that every other calendar is measured against, so a given
-- instant's Gregorian date is the reference that the other calendars are offset from.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Gregorian
(
  -- * Constructors
   calendarDate
  ,fromNthDay
  ,fromWeekDate
  -- * Types
  ,Month(..)
  ,DayOfWeek(..)
  ,Gregorian
)
where

import Data.HodaTime.Calendar.Gregorian.Internal hiding (fromWeekDate)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDate, DayNth, DayOfMonth, Year, WeekNumber)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI
import Control.Monad (guard)

-- Constructors

-- TODO: smart constructors hard coded to Maybe, make them like LocalTime

-- | Smart constructor for a 'Gregorian' calendar date.  Returns 'Nothing' if the day is out of range for the given month and year, or if the date falls before the 15 October 1582 changeover.
calendarDate :: DayOfMonth -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
calendarDate d m y = do
  guard $ d > 0 && d <= maxDaysInMonth m y
  let gd = gregorianFromYmd y m d
  guard $ gregorianToDays gd > invalidDayThresh
  return gd

-- | Smart constructor for a 'Gregorian' calendar date given as a day relative to a month (e.g. the third Monday of the month).  Returns 'Nothing' if the resulting date is invalid.
fromNthDay :: DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromNthDay nth dow m y = do
  guard $ d > 0 && d <= mdim
  guard $ days > invalidDayThresh
  return $ daysToGregorian (fromIntegral days)
  where
    nth' = fromEnum nth - 4
    mdim = maxDaysInMonth m y
    d = nthDayToDayOfMonth nth' (fromEnum dow) m y
    days = yearMonthDayToDays y m d

-- | Smart constructor for a 'Gregorian' calendar date given as a week date.  Note that this method assumes weeks start on Sunday and the first week of the year is the one
--   which has at least one day in the new year.  For ISO compliant behavior use this constructor from the ISO module
fromWeekDate :: WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromWeekDate = GI.fromWeekDate 1 Sunday