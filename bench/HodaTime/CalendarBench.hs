{-# LANGUAGE FlexibleContexts #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Data.Maybe (fromJust)

import Data.HodaTime.CalendarDate (HasDate, MoY, DoW, day, month, year, dayOfWeek, next)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), DayOfWeek(..))
import Data.HodaTime.Period (applyPeriod, days)

-- | Force a date down to a single Int through the public accessors, so 'nf' evaluates the full decode.
forceDate :: (HasDate d, Enum (MoY d), Enum (DoW d)) => d -> Int
forceDate x = day x + 100 * fromEnum (month x) + 10000 * year x + 1000000 * fromEnum (dayOfWeek x)

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar (Gregorian)"
  [
     bench "construct"             $ nf (maybe 0 forceDate . calendarDate 15 June) 2020
    ,bench "decode"                $ nf forceDate cd
    ,bench "read month only"       $ nf (fromEnum . month) cd
    ,bench "read dayOfWeek only"   $ nf (fromEnum . dayOfWeek) cd
    ,bench "addDays in-century"    $ nf (forceDate . applyPeriod (days 40)) cd
    ,bench "addDays cross-century" $ nf (forceDate . applyPeriod (days 40000)) cd
    ,bench "next dow"              $ nf (forceDate . next 3 Monday) cd
  ]
  where
    cd = fromJust $ calendarDate 15 June 2020
