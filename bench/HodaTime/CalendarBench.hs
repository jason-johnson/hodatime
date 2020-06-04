{-# LANGUAGE DeriveGeneric #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), fromNthDay, Gregorian, daysToYearMonthDay, daysToYearMonthDay')
import Data.HodaTime.CalendarDate (DayNth(..))

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar Benchmarks" [fromNthBenches, daysToYearMonthDayBenches, daysToYearMonthDayPrimeBenches]

fromNthBenches :: Benchmark
fromNthBenches = bgroup "fromNth" [
   bench "Forward"  $ nf (fromNthDay Second Wednesday November) 2017
  ,bench "Backwards"  $ nf (fromNthDay SecondToLast Wednesday November) 2017
  ]

daysToYearMonthDayBenches :: Benchmark
daysToYearMonthDayBenches = bgroup "daysToYearMonthDay" [
   bench "0" $ nf daysToYearMonthDay 0
  ,bench "4759" $ nf daysToYearMonthDay 4759
  ,bench "59065" $ nf daysToYearMonthDay 59065
  ,bench "4759065" $ nf daysToYearMonthDay 4759065
  ,bench "-475945" $ nf daysToYearMonthDay (-475945)
  ]

daysToYearMonthDayPrimeBenches :: Benchmark
daysToYearMonthDayPrimeBenches = bgroup "daysToYearMonthDay Prime" [
   bench "0" $ nf daysToYearMonthDay' 0
  ,bench "4759" $ nf daysToYearMonthDay' 4759
  ,bench "59065" $ nf daysToYearMonthDay' 59065
  ,bench "4759065" $ nf daysToYearMonthDay' 4759065
  ,bench "-475945" $ nf daysToYearMonthDay' (-475945)
  ]

