-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.CalendarDate
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDateTime'.  A 'CalendarDateTime' represents a date and time within the calendar system that is part of its type.  It has no reference to a particular time zone and is therefor not
-- a globally unique value as June 3rd 2020 10:05pm occurred at different 'Instant's around the world.
--
-- === Construction
--
-- To construct one of these types you will need a 'CalendarDate' and a 'LocalTime'
----------------------------------------------------------------------------
module Data.HodaTime.CalendarDateTime
(
  -- * Types
   CalendarDateTime
  ,IsCalendar(..)
  ,HasDate(..)
  -- * Constructors
  ,on
  ,at
  ,atStartOfDay
)
where

import Data.HodaTime.CalendarDateTime.Internal
import Data.HodaTime.LocalTime.Internal (midnight)

-- | Returns a 'CalendarDateTime' at 'LocalTime' on the given 'CalendarDate'
on :: LocalTime -> CalendarDate cal -> CalendarDateTime cal
on = flip CalendarDateTime

-- | Returns the first valid time in the day specified by 'CalendarDate' within the given 'TimeZone'
atStartOfDay :: CalendarDate cal -> CalendarDateTime cal
atStartOfDay =  flip at midnight