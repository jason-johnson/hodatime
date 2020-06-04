{-# LANGUAGE DeriveGeneric #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.HodaTime.Calendar.Gregorian (Month(..), DayOfWeek(..), fromNthDay, Gregorian)
import Data.HodaTime.CalendarDate (DayNth(..))

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar Benchmarks" [fromNthBench, fromNthBenchBackwards]

fromNthBench :: Benchmark
fromNthBench = bench "fromNthBench"  $ nf (fromNthDay Second Wednesday November) 2017

fromNthBenchBackwards :: Benchmark
fromNthBenchBackwards = bench "fromNthBench Backwards"  $ nf (fromNthDay SecondToLast Wednesday November) 2017