module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.HodaTime.Calendar.Gregorian.Internal (maxDaysInMonth, daysInMonth)
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), fromNthDay, fromNthDay')
import Data.HodaTime.CalendarDate (DayNth(..))

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar Benchmarks" [maxDaysInMonthBench, daysInMonthBench, fromNthBench, fromNthPrimeBench]

maxDaysInMonthBench :: Benchmark
maxDaysInMonthBench = bench "maxDaysInMonthBench"  $ whnf (maxDaysInMonth November) 2017

daysInMonthBench :: Benchmark
daysInMonthBench = bench "daysInMonthBench"  $ whnf (daysInMonth 10) 2017

fromNthBench :: Benchmark
fromNthBench = bench "fromNthBench"  $ nf (fromNthDay Second Wednesday November) 2017

fromNthPrimeBench :: Benchmark
fromNthPrimeBench = bench "fromNthPrimeBench"  $ nf (fromNthDay' Second Wednesday November) 2017
