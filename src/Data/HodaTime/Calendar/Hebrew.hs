-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Hebrew
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Hebrew' calendar.
----------------------------------------------------------------------------
module Data.HodaTime.Calendar.Hebrew
(
)
where

data HebrewMonthNumbering =
    HebrewCivil
  | HebrewScriptural
  deriving (Eq, Show)