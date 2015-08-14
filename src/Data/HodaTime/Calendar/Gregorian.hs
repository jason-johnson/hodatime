module Data.HodaTime.Calendar.Gregorian
(
   WeekDay(..)
  ,Month(..)
  ,localDate
)
where

import Data.HodaTime.Calendar.Gregorian.Internal
import Data.HodaTime.Calendar(Calendar(..))
import Data.HodaTime.Constants (daysPerYear, monthDayOffsets)
import Data.HodaTime.LocalDateTime.Internal (LocalDate(..))
import Data.Int (Int32)

-- types

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq, Ord, Enum, Bounded)

-- constructors

localDate :: Int -> Month -> Int -> LocalDate
localDate year month day
    | dateIsValid = LocalDate (fromIntegral year) (fromIntegral . fromEnum $ month) (fromIntegral day) Gregorian
    | otherwise = error "Invalid date"
    where
        dateIsValid = True              -- TODO: Implement this

-- helper functions

yearMonthDayToDays :: Int -> Int -> Int -> Int32
yearMonthDayToDays year month day = fromIntegral days
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1