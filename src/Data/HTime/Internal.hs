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

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

data Month = January | February | March | April | May | June | July | August | September | October | November | December






toDateTime :: Int -> Int -> Int -> Word -> Word -> Word -> Word -> DateTime
toDateTime year month day hour minute second = DateTime days secs
  where
    month' = if month > 1 then month - 2 else month + 10
    years = if month < 2 then year - 2001 else year - 2000
    secs = hour * secondsPerHour + minute * minutesPerHour + second
    yearDays = years * daysPerYear + years `div` 4 + years `div` 400 - years `div` 100
    days = yearDays + monthDayOffsets !! month' + day - 1