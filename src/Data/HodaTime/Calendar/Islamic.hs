-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Calendar.Islamic
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'CalendarDate' and 'CalendarDateTime' in the 'Islamic' calendar.
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