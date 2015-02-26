module Data.HTime.Internal
(
   Day(..)
  ,Month(..)
  ,DateTime(..)
  ,Date(..)
  ,toDateTime
  ,toDate
  ,enumFromDate
  ,enumFromToDate
)
where

import Data.Word (Word)
import Data.HTime.Constants
import Control.Arrow ((>>>), (&&&), (***), first)
import Data.List (findIndex)
import Data.Maybe (fromJust)

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

-- types

data DateTime = DateTime { dtDays :: Int, dtSecs :: Word, dtNsecs :: Word }
  deriving (Eq, Ord)

newtype Date = Date DateTime

-- smart constructors

dateToDays :: Int -> Int -> Int -> Int
dateToDays year month day = days
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1

toDate :: Int -> Month -> Int -> Date
toDate year month day = Date $ DateTime days 0 0
  where
    days = dateToDays year (toInt month) day

toDateTime :: Int -> Month -> Int -> Word -> Word -> Word -> Word -> DateTime
toDateTime year month day hour minute second = DateTime days secs
  where
    days = dateToDays year (toInt month) day
    secs = hour * secondsPerHour + minute * minutesPerHour + second

-- enumerating

enumFromDate :: Date -> [Date]
enumFromDate d@(Date (DateTime days _ _)) = d : ds
  where
    ds = enumFromDate . Date $ DateTime (succ days) 0 0

enumFromToDate :: Date -> Date -> [Date]
enumFromToDate d@(Date (DateTime days _ _)) d'@(Date (DateTime days' _ _)) = d : ds
  where
    ds
      | days == days' = []
      | days > days'  = enumFromToDate (Date $ DateTime (pred days) 0 0) d'
      | otherwise     = enumFromToDate (Date $ DateTime (succ days) 0 0) d'

-- decoding

daysToDate :: Int -> (Int, Month, Int, Bool)
daysToDate days = (year, fromInt month'', day', isLeapDay)
  where
    (cycleYears, (cycleDays, isLeapDay)) = flip divMod daysPerCycle >>> (* 400) *** id &&& borders daysPerCycle $ days
    (centuryYears, centuryDays) = flip divMod daysPerCentury >>> first (* 100) $ cycleDays
    (fourYears, (remaining, isLeapDay')) = flip divMod daysPerFourYears >>> (* 4) *** id &&& borders daysPerFourYears $ centuryDays
    (oneYears, yearDays) = remaining `divMod` daysPerYear
    month = pred . fromJust . findIndex (\y -> yearDays < y) $ monthDayOffsets
    (month', startDate) = if month >= 10 then (month - 10, 2001) else (month + 2, 2000)
    day = yearDays - monthDayOffsets !! month + 1
    (month'', day') = if isLeapDay || isLeapDay' then (1, 29) else (month', day)
    year = startDate + cycleYears + centuryYears + fourYears + oneYears
    borders c x = x == c - 1

decodeDate' :: Date -> (Int, Month, Int, Bool)
decodeDate' (Date (DateTime days _ _)) = daysToDate days

decodeDateTime' :: DateTime -> (Int, Month, Int, Word, Word, Word, Word)
decodeDateTime' (DateTime days secs nsecs) = (year, month, day, hour, minute, sec, nsecond)
  where
    (year, month, day, _) = daysToDate days
    (hour, secs') = secs `divMod` secondsPerHour
    (minute, sec) = secs' `divMod` minutesPerHour
    nsecond = nsecs