module Data.HTime.Internal
(
   Day(..)
  ,Month(..)
  ,DateTime(..)
  ,mkCache
  ,toDateTime
)
where

import Data.Int (Int16)
import Data.Word (Word)
import Data.HTime.Constants

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

data Month = January | February | March | April | May | June | July | August | September | October | November | December

data DateTime = DateTime { dtDays :: Int, dtSecs :: Word, dtNsecs :: Word }

type DTCacheTableDaysEntry = Int16
type DTCacheTableHoursEntry = Int16

data DTCacheTable = DTCacheTable [DTCacheTableDaysEntry] [DTCacheTableDaysEntry] [DTCacheTableHoursEntry]

mkCache :: DTCacheTable
mkCache = DTCacheTable days negDays hours where
  days = epoch : nextDays
  negDays = prev : prevDays
  hours = midnight : nextHours

  epoch = undefined
  nextDays = undefined
  prev = undefined
  prevDays = undefined
  midnight = undefined
  nextHours = undefined

toDateTime :: Int -> Int -> Int -> Word -> Word -> Word -> Word -> DateTime
toDateTime year month day hour minute second = DateTime days secs
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    secs = hour * secondsPerHour + minute * minutesPerHour + second
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1