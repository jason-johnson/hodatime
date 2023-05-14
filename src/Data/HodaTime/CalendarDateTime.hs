module Data.HodaTime.CalendarDateTime
(
  -- * Types
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate
  ,CalendarDateTime
  ,IsCalendar(..)
  ,HasDate(..)
  ,LocalTime
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