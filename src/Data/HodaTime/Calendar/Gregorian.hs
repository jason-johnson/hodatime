module Data.HodaTime.Calendar.Gregorian
(
   WeekDay(..)
  ,Month(..)
  ,localDate
  ,fromInstant
  ,next
  ,previous
)
where

import Data.HodaTime.Calendar.Gregorian.Internal
import Data.HodaTime.Calendar(Calendar(..))
import Data.HodaTime.Constants (daysPerYear, monthDayOffsets)
import Data.HodaTime.LocalDateTime.Internal (LocalDate(..))
import Data.Int (Int32)
import Data.HodaTime.Instant.Internal (Instant(..))

-- types

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq, Ord, Enum, Bounded)

-- constructors

localDate :: Int -> Month -> Int -> Maybe LocalDate           -- TODO: There should be a function that takes the month as a number and applies the calendar data to determine if date is valid, this will call that function
localDate year month day
    | dateIsValid = Just $ LocalDate (fromIntegral year) (fromIntegral . fromEnum $ month) (fromIntegral day) Gregorian
    | otherwise = Nothing
    where
        dateIsValid = True              -- TODO: Implement this

-- interface

next :: WeekDay -> LocalDate              -- TODO: Make these methods in a type class as they should apply to LocalDate and LocalDateTime
next = undefined

previous :: WeekDay -> LocalDate
previous = undefined

-- helper functions

fromInstant :: Instant -> LocalDate         -- TODO: Should this be here?  Better yet, should such a thing even exist?
fromInstant = flip fromInstantInCalendar Gregorian

yearMonthDayToDays :: Int -> Int -> Int -> Int32
yearMonthDayToDays year month day = fromIntegral days
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1
