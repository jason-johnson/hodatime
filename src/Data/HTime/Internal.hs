module Data.HTime.Internal
(
   Day(..)
  ,Month(..)
  ,DateTime(..)
  ,toDateTime
)
where

import Data.Word (Word)
import Data.HTime.Constants
import Control.Arrow ((>>>), first)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq, Ord, Enum, Bounded)

class IntConverter a where
  toInt :: a -> Int
  fromInt :: Int -> a

instance IntConverter Day where
  toInt Sunday = 0
  toInt Monday = 1
  toInt Tuesday = 2
  toInt Wednesday = 3
  toInt Thursday = 4
  toInt Friday = 5
  toInt Saturday = 6
  fromInt 0 = Sunday
  fromInt 1 = Monday
  fromInt 2 = Tuesday
  fromInt 3 = Wednesday
  fromInt 4 = Thursday
  fromInt 5 = Friday
  fromInt 6 = Saturday
  fromInt n = error $ "invalid day int: " ++ show n 

instance IntConverter Month where
  toInt January = 0
  toInt February = 1
  toInt March = 2
  toInt April = 3
  toInt May = 4
  toInt June = 5
  toInt July = 6
  toInt August = 7
  toInt September = 8
  toInt October = 9
  toInt November = 10
  toInt December = 11
  fromInt 0 = January
  fromInt 1 = February
  fromInt 2 = March
  fromInt 3 = April
  fromInt 4 = May
  fromInt 5 = June
  fromInt 6 = July
  fromInt 7 = August
  fromInt 8 = September
  fromInt 9 = October
  fromInt 10 = November
  fromInt 11 = December
  fromInt n = error $ "invalid month in: " ++ show n

data DateTime = DateTime { dtDays :: Int, dtSecs :: Word, dtNsecs :: Word }
  deriving (Eq, Ord)

toDateTime :: Int -> Month -> Int -> Word -> Word -> Word -> Word -> DateTime
toDateTime year month = toDateTime' year (toInt month)

toDateTime' :: Int -> Int -> Int -> Word -> Word -> Word -> Word -> DateTime
toDateTime' year month day hour minute second = DateTime days secs
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    secs = hour * secondsPerHour + minute * minutesPerHour + second
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1

decodeDate' :: DateTime -> (Int, Month, Int, Word, Word, Word, Word)
decodeDate' (DateTime days secs nsecs) = (year, fromInt month', day, hour, minute, sec, nsecond)
  where
    (cycleYears, cycleDays) = flip divMod daysPerCycle >>> first (* 400) $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    (fourYears, remaining) = flip divMod 1461 >>> first (* 4) $ centuryDays
    (oneYears, yearDays) = remaining `divMod` 365
    month = pred . fromMaybe 100 . findIndex (\ y -> yearDays < y) $ monthDayOffsets
    (month', startDate) = if month >= 10 then (month - 10, 2001) else (month + 2, 2000)
    day = yearDays - monthDayOffsets !! month + 1
    year = startDate + cycleYears + centuryYears + fourYears + oneYears
    (hour, secs') = secs `divMod` secondsPerHour
    (minute, sec) = secs' `divMod` minutesPerHour
    nsecond = nsecs