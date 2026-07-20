{-# LANGUAGE FlexibleContexts #-}
module HodaTime.CalendarBench
(
  calendarBenches
)
where

import Criterion.Main
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)

import Data.HodaTime.CalendarDate (HasDate, MoY, DoW, day, month, year, dayOfWeek, next, yearMonthDay)
import Data.HodaTime.Calendar.Gregorian (calendarDate, ncalendarDate, Month(..), DayOfWeek(..))

-- Minimal van Laarhoven lens helpers (same as the test suite).  Kept local so the benchmark depends only on the
-- public library interface and never reaches into internal modules.
get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const

modify :: (s -> b) -> ((s -> Identity b) -> a -> Identity t) -> a -> t
modify f l = runIdentity . l (Identity . f)

-- | Force a date down to a single Int through the public accessors.  The calendar types have lazy fields, so
--   'whnf' on a constructor would leave the real work as thunks; reducing to an Int makes 'nf' evaluate it all.
forceDate :: (HasDate d, Enum (MoY d), Enum (DoW d)) => d -> Int
forceDate x = get day x + 100 * fromEnum (month x) + 10000 * get year x + 1000000 * fromEnum (dayOfWeek x)

-- | Same three date components as 'forceDate' (day, month, year) but decoded in one pass via 'yearMonthDay'.
--   For a packed representation this decodes the value once instead of once per accessor.
forceYMD :: (HasDate d, Enum (MoY d)) => d -> Int
forceYMD x = d + 100 * fromEnum m + 10000 * y
  where (y, m, d) = yearMonthDay x

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar (CalendarDate vs NCalendarDate)"
  [
     bgroup "construct"
       [ bench "CalendarDate"  $ nf (maybe 0 forceDate . calendarDate 15 June) 2020
       , bench "NCalendarDate" $ nf (maybe 0 forceDate . ncalendarDate 15 June) 2020
       ]
    ,bgroup "decode"
       [ bench "CalendarDate"  $ nf forceDate cd
       , bench "NCalendarDate" $ nf forceDate ncd
       ]
    ,bgroup "decode (yearMonthDay)"
       [ bench "CalendarDate"  $ nf forceYMD cd
       , bench "NCalendarDate" $ nf forceYMD ncd
       ]
    ,bgroup "read month only"
       [ bench "CalendarDate"  $ nf (fromEnum . month) cd
       , bench "NCalendarDate" $ nf (fromEnum . month) ncd
       ]
    ,bgroup "read dayOfWeek only"
       [ bench "CalendarDate"  $ nf (fromEnum . dayOfWeek) cd
       , bench "NCalendarDate" $ nf (fromEnum . dayOfWeek) ncd
       ]
    ,bgroup "addDays in-century"
       [ bench "CalendarDate"  $ nf (forceDate . modify (+ 40) day) cd
       , bench "NCalendarDate" $ nf (forceDate . modify (+ 40) day) ncd
       ]
    ,bgroup "addDays cross-century"
       [ bench "CalendarDate"  $ nf (forceDate . modify (+ 40000) day) cd
       , bench "NCalendarDate" $ nf (forceDate . modify (+ 40000) day) ncd
       ]
    ,bgroup "next dow"
       [ bench "CalendarDate"  $ nf (forceDate . next 3 Monday) cd
       , bench "NCalendarDate" $ nf (forceDate . next 3 Monday) ncd
       ]
  ]
  where
    cd = fromJust $ calendarDate 15 June 2020
    ncd = fromJust $ ncalendarDate 15 June 2020
