-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.LocalTime
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'LocalTime' represents a time of day, with no reference to a particular calendar, time zone or date.
-- This module contains constructors and functions for working with 'LocalTime'.
--
-- === Normalization
--
-- A clock time can be from 00:00:00.000 to 23:59:59.99.. Adding 1 minute to 23:59:00 will cause it to roll over to 00:00:00.
----------------------------------------------------------------------------
module Data.HodaTime.LocalTime
(
   LocalTime
  ,HasLocalTime(..)
  ,Hour
  ,Minute
  ,Second
  ,Nanosecond
  ,localTime
)
where

import Data.HodaTime.LocalTime.Internal