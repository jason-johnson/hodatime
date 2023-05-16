-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Julian
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Julian' calendar.  The calendar is proleptic in that, while it does start in 45 BC it does not try to account for the fact
-- that before around 4 AD the leap year rule was accidentally implemented as a leap year every three years.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Julian
(
  -- * Constructors
--   calendarDate
--  ,fromNthDay
--  ,fromWeekDate
  -- * Types
--  ,Month(..)
--  ,DayOfWeek(..)
--  ,Gregorian
)
where