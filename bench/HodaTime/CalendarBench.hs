{-# LANGUAGE DeriveGeneric #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), fromNthDay, fromNthDay', Gregorian)
import Data.HodaTime.CalendarDate (DayNth(..))

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar Benchmarks" [fromNthBench, fromNthPrimeBench, fromNthBenchBackwards, fromNthPrimeBenchBackwards]

fromNthBench :: Benchmark
fromNthBench = bench "fromNthBench"  $ nf (fromNthDay Second Wednesday November) 2017

fromNthPrimeBench :: Benchmark
fromNthPrimeBench = bench "fromNthPrimeBench"  $ nf (fromNthDay' Second Wednesday November) 2017

fromNthBenchBackwards :: Benchmark
fromNthBenchBackwards = bench "fromNthBench Backwards"  $ nf (fromNthDay SecondToLast Wednesday November) 2017

fromNthPrimeBenchBackwards :: Benchmark
fromNthPrimeBenchBackwards = bench "fromNthPrimeBench Backwards"  $ nf (fromNthDay' SecondToLast Wednesday November) 2017