-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Hebrew
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Hebrew' (Jewish) calendar, a lunisolar calendar that keeps its months in step with the solar year by inserting a leap month
-- seven times in each 19-year Metonic cycle.  Months can be counted in either the civil or the scriptural convention (see 'HebrewMonthNumbering').
--
-- Note: this calendar is not yet implemented.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Hebrew
(
)
where

data HebrewMonthNumbering =
    HebrewCivil
  | HebrewScriptural
  deriving (Eq, Show)