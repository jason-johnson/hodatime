{-# LANGUAGE DeriveGeneric #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.HodaTime.Calendar.Gregorian.Internal (maxDaysInMonth, daysInMonth)
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), fromNthDay, fromNthDay', Gregorian)
import Data.HodaTime.CalendarDate (DayNth(..))
import Data.Maybe (fromJust)

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar Benchmarks" [maxDaysInMonthBench, daysInMonthBench, fromNthBench, fromNthPrimeBench, fromNthBenchBackwards, fromNthPrimeBenchBackwards]

maxDaysInMonthBench :: Benchmark
maxDaysInMonthBench = bench "maxDaysInMonthBench"  $ whnf (maxDaysInMonth November) 2017

daysInMonthBench :: Benchmark
daysInMonthBench = bench "daysInMonthBench"  $ whnf (daysInMonth 10) 2017

fromNthBench :: Benchmark
fromNthBench = bench "fromNthBench"  $ nf (fromNthDay Second Wednesday November) 2017

fromNthPrimeBench :: Benchmark
fromNthPrimeBench = bench "fromNthPrimeBench"  $ nf (fromNthDay' Second Wednesday November) 2017

fromNthBenchBackwards :: Benchmark
fromNthBenchBackwards = bench "fromNthBench Backwards"  $ nf (fromNthDay SecondToLast Wednesday November) 2017

fromNthPrimeBenchBackwards :: Benchmark
fromNthPrimeBenchBackwards = bench "fromNthPrimeBench Backwards"  $ nf (fromNthDay' SecondToLast Wednesday November) 2017