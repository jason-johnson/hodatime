-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.LocalTime
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'LocalTime' represents a local clock time.  This module contains constructors and functions for working with 'LocalTime'.
--
-- === Normalization
--
-- A clock time can be from 00:00:00.000 to 23:59:59.99.. Adding 1 minute to 23:59:00 will cause it to roll over to 00:00:00.
----------------------------------------------------------------------------
module Data.HodaTime.LocalTime
(
   fromTime
  ,fromTime'
  ,hour
  ,minute
  ,second
)
where

import Data.HodaTime.LocalTime.Internal
import Data.HodaTime.Constants (secondsPerHour)
import Data.Word (Word32)

secsFromHours :: Int -> Word32
secsFromHours = (* secondsPerHour) . fromIntegral

secsFromMinutes :: Int -> Word32
secsFromMinutes = (* 60) . fromIntegral

-- Construction

-- | Create a new 'LocalTime' from an hour, minute and second
fromTime :: Int -> Int -> Int -> LocalTime
fromTime h m s = LocalTime (h' + m' + fromIntegral s) 0
  where
    h' = secsFromHours h
    m' = secsFromMinutes m

fromTime' :: Int -> Int -> Int -> Int -> LocalTime
fromTime' h m s ns = LocalTime (h' + m' + fromIntegral s) (fromIntegral ns)
  where
    h' = secsFromHours h
    m' = secsFromMinutes m

-- Accessors

hour :: LocalTime -> Int
hour (LocalTime secs _) = fromIntegral h
  where
    h = secs `div` secondsPerHour

minute :: LocalTime -> Int
minute (LocalTime secs _) = fromIntegral m
  where
    s = secs `mod` secondsPerHour
    m = s `div` 60

second :: LocalTime -> Int
second (LocalTime secs _) = fromIntegral s'
  where
    s = secs `mod` secondsPerHour
    s' = s `mod` 60
