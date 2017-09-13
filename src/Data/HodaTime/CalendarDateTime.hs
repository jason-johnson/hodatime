module Data.HodaTime.CalendarDateTime
(
   DayNth(..)
  ,Year
  ,WeekNumber
  ,DayOfMonth
  ,CalendarDate
  ,CalendarDateTime
  ,IsCalendar(..)
  ,HasDate(..)
  ,LocalTime
  ,on
  ,at
)
where

import Data.HodaTime.CalendarDateTime.Internal

on :: LocalTime -> CalendarDate cal -> CalendarDateTime cal
on time date = CalendarDateTime date time

at :: CalendarDate cal -> LocalTime -> CalendarDateTime cal
at date time = CalendarDateTime date time