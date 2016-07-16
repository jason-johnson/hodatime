{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Calendar.Gregorian
(
   calendarDate
  ,fromNthDay
  ,Month(..)
  ,DayOfWeek(..)
)
where

import Data.HodaTime.Calendar.Gregorian.Internal
import Data.HodaTime.Calendar.Internal
import Data.HodaTime.Constants (daysPerYear, monthDayOffsets)
import Data.Int (Int32)
import Control.Monad (guard)

minDate :: Int
minDate = 1582

-- types

data Gregorian

instance IsCalendar Gregorian where
  type Date Gregorian = CalendarDate Int Gregorian
  data DayOfWeek Gregorian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Eq, Ord, Enum, Bounded)
  data Month Gregorian = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Eq, Ord, Enum, Bounded)
  type CalendarOptions Gregorian = Int

  next' = undefined
  previous' = undefined

-- | Smart constuctor for Gregorian calendar date.  Minimum first week days is the first argument to allow for easier defaulting
calendarDate :: Int -> Int -> Month Gregorian -> Int -> Maybe (Date Gregorian)
calendarDate minFirstWeekDays d m y = do
  guard $ y > minDate
  guard $ d > 0 && d <= maxDaysInMonth (fromEnum m) y
  let days = yearMonthDayToDays y m d
  return $ CalendarDate days minFirstWeekDays

fromNthDay :: Int -> DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Int -> Maybe (Date Gregorian)
fromNthDay minFirstWeekDays nth dow m y = flip CalendarDate minFirstWeekDays <$> days
  where
    mdim = maxDaysInMonth (fromEnum m) y
    fomDays = yearMonthDayToDays y m 1
    fdow = fomDays `mod` 7 + fromEnum' Wednesday
    fdow' = if fdow > 6 then fdow - 7 else fdow
    tdow = fromEnum' dow
    tdow' = if tdow >= fdow' then tdow else 7 + tdow
    dom = tdow' - fdow'
    adj First = dom
    adj Second = dom + 7
    adj Third = dom + 14
    adj Fourth = dom + 21
    adj Fifth = dom + 28
    dom' = adj nth
    days = if dom' < fromIntegral mdim then Just (fomDays + dom') else Nothing    -- NOTE: we have to use < not <= because we're adding to first of the month
    fromEnum' = fromIntegral . fromEnum

-- helper functions

maxDaysInMonth :: Int -> Int -> Int
maxDaysInMonth 1 y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap
      | 0 == y `mod` 100                  = 0 == y `mod` 400
      | otherwise                         = 0 == y `mod` 4
maxDaysInMonth n _
  | n == 3 || n == 5 || n == 8 || n == 10 = 30
  | otherwise                             = 31

yearMonthDayToDays :: Int -> Month Gregorian -> Int -> Int32
yearMonthDayToDays year month day = fromIntegral days
  where
    m = fromEnum month
    month' = if m > 1 then m - 2 else m + 10
    years = if m < 2 then year - 2001 else year - 2000
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1
