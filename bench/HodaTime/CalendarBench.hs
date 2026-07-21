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

import Data.HodaTime.CalendarDate (HasDate, MoY, DoW, day, month, year, dayOfWeek, next)
import Data.HodaTime.Calendar.Gregorian (calendarDate, Month(..), DayOfWeek(..))

-- Minimal van Laarhoven lens helpers (same as the test suite).  Kept local so the benchmark depends only on the
-- public library interface and never reaches into internal modules.
get :: ((s -> Const s c) -> a -> Const t b) -> a -> t
get l = getConst . l Const

modify :: (s -> b) -> ((s -> Identity b) -> a -> Identity t) -> a -> t
modify f l = runIdentity . l (Identity . f)

-- | Force a date down to a single Int through the public accessors, so 'nf' evaluates the full decode.
forceDate :: (HasDate d, Enum (MoY d), Enum (DoW d)) => d -> Int
forceDate x = get day x + 100 * fromEnum (month x) + 10000 * get year x + 1000000 * fromEnum (dayOfWeek x)

calendarBenches :: Benchmark
calendarBenches = bgroup "Calendar (Gregorian)"
  [
     bench "construct"             $ nf (maybe 0 forceDate . calendarDate 15 June) 2020
    ,bench "decode"                $ nf forceDate cd
    ,bench "read month only"       $ nf (fromEnum . month) cd
    ,bench "read dayOfWeek only"   $ nf (fromEnum . dayOfWeek) cd
    ,bench "addDays in-century"    $ nf (forceDate . modify (+ 40) day) cd
    ,bench "addDays cross-century" $ nf (forceDate . modify (+ 40000) day) cd
    ,bench "next dow"              $ nf (forceDate . next 3 Monday) cd
  ]
  where
    cd = fromJust $ calendarDate 15 June 2020
