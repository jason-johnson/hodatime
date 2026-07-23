-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Islamic
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Islamic' (Hijri) calendar, a purely lunar calendar of twelve alternating 30- and 29-day months.  Its year is about eleven days
-- shorter than the solar year, so its dates drift through the seasons.  The tabular form approximates the observed calendar with a 30-year leap cycle; the exact leap years and the starting epoch are
-- selectable (see 'IslamicLeapYearPattern' and 'IslamicEpoch').
--
-- Note: this calendar is not yet implemented.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Islamic
(
)
where

data IslamicLeapYearPattern =
    ILYPBase15
  | ILYPBase16
  | ILYPIndian
  | ILYPHabashAlHasib
  deriving (Eq, Show)
    
data IslamicEpoch =
    IslamicAstronomical
  | IslamicCivil
  deriving (Eq, Show)