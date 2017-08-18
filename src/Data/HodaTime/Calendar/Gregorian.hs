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
import Control.Monad (guard)

minDate :: Int
minDate = 1582

epochDayOfWeek :: DayOfWeek Gregorian
epochDayOfWeek = Wednesday

-- types

data Gregorian

instance IsCalendar Gregorian where
  type Date Gregorian = CalendarDate Gregorian
  data DayOfWeek Gregorian = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Eq, Ord, Enum, Bounded)
  data Month Gregorian = January | February | March | April | May | June | July | August | September | October | November | December      -- TODO: Move January and February to the back so the Enums work without adjustment
    deriving (Show, Eq, Ord, Enum, Bounded)                                                                                               -- TODO: Note that if we do this, we can't derive Ord and possibly not bounded, they have to be hand written (is this true?  Do we need Jan == 0?)
  dayOfWeek' (CalendarDate days) = toEnum . dayOfWeekFromDays . fromIntegral $ days

  next' = undefined
  previous' = undefined

-- | Smart constuctor for Gregorian calendar date.  Minimum first week days is the first argument to allow for easier defaulting
calendarDate :: Int -> Int -> Month Gregorian -> Int -> Maybe (Date Gregorian)
calendarDate minFirstWeekDays d m y = do
  guard $ y > minDate
  guard $ d > 0 && d <= maxDaysInMonth m y
  let days = fromIntegral $ yearMonthDayToDays y m d
  return $ CalendarDate days

fromNthDay :: Int -> DayNth -> DayOfWeek Gregorian -> Month Gregorian -> Int -> Maybe (Date Gregorian)
fromNthDay minFirstWeekDays nth dow m y = CalendarDate <$> go (fromEnum nth)
  where
    go d
      | d < 5     = forward d
      | otherwise = backward (d - 5)
    mdim = maxDaysInMonth m y
    fomDays = yearMonthDayToDays y m 1
    eomDays = yearMonthDayToDays y m mdim
    toDays days adjust f multiple =
      let
        startDow = dayOfWeekFromDays days
        targetDow = fromEnum dow
        adjustment = 7 * multiple + adjust startDow targetDow
        days' = days `f` adjustment
        in do
          guard $ adjustment < fromIntegral mdim           -- NOTE: we have to use < not <= because we're adding to first of the month or subtracting from the end of the month
          return $ fromIntegral days'
    forward = toDays fomDays weekdayDistance (+)
    backward = toDays eomDays (flip weekdayDistance) (-)

-- helper functions

dayOfWeekFromDays :: Int -> Int
dayOfWeekFromDays = normalize . (fromEnum epochDayOfWeek +) . flip mod 7
  where
    normalize n = if n >= 7 then n - 7 else n

weekdayDistance :: (Ord a, Num a) => a -> a -> a
weekdayDistance s e = e' - s
  where
    e' = if e >= s then e else e + 7

maxDaysInMonth :: Month Gregorian -> Int -> Int
maxDaysInMonth February y
  | isLeap                                = 29
  | otherwise                             = 28
  where
    isLeap
      | 0 == y `mod` 100                  = 0 == y `mod` 400
      | otherwise                         = 0 == y `mod` 4
maxDaysInMonth n _
  | n == April || n == June || n == September || n == November  = 30
  | otherwise                                                   = 31

yearMonthDayToDays :: Int -> Month Gregorian -> Int -> Int
yearMonthDayToDays year month day = days
  where
    m = fromEnum month
    month' = if m > 1 then m - 2 else m + 10
    years = if m < 2 then year - 2001 else year - 2000
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1
