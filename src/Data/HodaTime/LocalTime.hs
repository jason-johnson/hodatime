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
   LocalTime
  ,fromTime
  ,hours
  ,minutes
  ,seconds
  ,nanoseconds
)
where

import Data.HodaTime.LocalTime.Internal
import Data.HodaTime.Internal (hoursFromSecs, minutesFromSecs, secondsFromSecs, secondsFromHours, secondsFromMinutes)
import Control.Monad (guard)

-- Construction

-- | Create a new 'LocalTime' from an hour, minute, second and nanosecond if values are valid, nothing otherwise
fromTime :: Int -> Int -> Int -> Int -> Maybe LocalTime
fromTime h m s ns = do
  guard $ h < 24 && h >= 0
  guard $ m < 60 && m >= 0
  guard $ s < 60 && m >= 0
  guard $ ns >= 0
  return $ LocalTime (h' + m' + fromIntegral s) (fromIntegral ns)
  where
    h' = secondsFromHours h
    m' = secondsFromMinutes m

-- Accessors

-- | Lens for the hours component of the 'LocalTime'
hours :: Functor f => (Int -> f Int) -> LocalTime -> f LocalTime
hours f (LocalTime secs nsecs) = hoursFromSecs to f' secs
  where
    to = flip LocalTime nsecs
    normalize x = if x > 23 then x - 24 else x
    f' x = normalize <$> f x
{-# INLINE hours #-}

-- | Lens for the minutes component of the 'LocalTime'
minutes :: Functor f => (Int -> f Int) -> LocalTime -> f LocalTime
minutes f (LocalTime secs nsecs) = minutesFromSecs to f secs
  where
    to = flip LocalTime nsecs
{-# INLINE minutes #-}

-- | Lens for the seconds component of the 'LocalTime'
seconds :: Functor f => (Int -> f Int) -> LocalTime -> f LocalTime
seconds f (LocalTime secs nsecs) = secondsFromSecs to f secs
  where
    to = flip LocalTime nsecs
{-# INLINE seconds #-}

-- | Lens for the nanoseconds component of the 'LocalTime'.  NOTE: no effort is made to detect nano overflow.  They will simply roll over on overflow without affecting the rest of the time.
nanoseconds :: Functor f => (Int -> f Int) -> LocalTime -> f LocalTime
nanoseconds f (LocalTime secs nsecs) = LocalTime secs . fromIntegral <$> (f . fromIntegral) nsecs
{-# INLINE nanoseconds #-}
