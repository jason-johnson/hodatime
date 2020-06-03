-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Gregorian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Gregorian' calendar.
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
import Data.HodaTime.CalendarDateTime.Internal (CalendarDate(..), DayNth, DayOfMonth, Year, WeekNumber)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI
import Control.Monad (guard)

-- Constructors

-- | Smart constuctor for Gregorian calendar date.
calendarDate :: DayOfMonth -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
calendarDate d m y = do
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = fromIntegral $ yearMonthDayToDays y m d
  guard $ days > invalidDayThresh
  return $ CalendarDate days (fromIntegral d) (fromIntegral . fromEnum $ m) (fromIntegral y)

-- | Smart constuctor for Gregorian calendar date based on relative month day.
fromNthDay :: DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromNthDay nth dow m y = do
  guard $ d > 0 && d <= mdim
  guard $ days > invalidDayThresh
  return $ CalendarDate (fromIntegral days) (fromIntegral d) (fromIntegral . fromEnum $ m) (fromIntegral y)
  where
    nth' = fromEnum nth - 4
    mdim = maxDaysInMonth m y
    d = nthDayToDayOfMonth nth' (fromEnum dow) m y
    days = yearMonthDayToDays y m d

-- | Smart constuctor for Gregorian calendar date based on week date.  Note that this method assumes weeks start on Sunday and the first week of the year is the one
--   which has at least one day in the new year.  For ISO compliant behavior use this constructor from the ISO module
fromWeekDate :: WeekNumber -> DayOfWeek Gregorian -> Year -> Maybe (CalendarDate Gregorian)
fromWeekDate = GI.fromWeekDate 1 Sunday