-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Offset
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'Offset' is a period of time offset from UTC time.  This module contains constructors and functions for working with 'Offsets'.
--
-- === Clamping
--
-- An offset must be between 18 hours and -18 hours (inclusive).  If you go outside this range the functions will clamp to the nearest value.
----------------------------------------------------------------------------
module Data.HodaTime.Offset
(
  -- * Types
   Offset
  -- * Constructors
  ,empty
  ,fromSeconds
  ,fromMinutes
  ,fromHours
  -- * Lenses
  ,seconds
  ,minutes
  ,hours
  -- * Math
  ,addClamped
  ,minusClamped
  -- * Utils
  ,minOffsetSeconds
  ,maxOffsetSeconds
)
where

import Data.HodaTime.Offset.Internal
import Data.HodaTime.Internal (secondsFromMinutes, secondsFromHours, clamp, hoursFromSecs, minutesFromSecs, secondsFromSecs)

-- Offset specific constants

minOffsetHours :: Num a => a
minOffsetHours = negate maxOffsetHours

maxOffsetMinutes :: Num a => a
maxOffsetMinutes = maxOffsetHours * 60

minOffsetMinutes :: Num a => a
minOffsetMinutes = negate maxOffsetMinutes

-- | Create an 'Offset' of (clamped) m minutes.
fromMinutes :: Integral a => a -> Offset
fromMinutes = Offset . secondsFromMinutes . clamp minOffsetMinutes maxOffsetMinutes

-- | Create an 'Offset' of (clamped) h hours.
fromHours :: Integral a => a -> Offset
fromHours = Offset . secondsFromHours . clamp minOffsetHours maxOffsetHours

-- | Lens for the seconds component of the 'Offset'
seconds :: Functor f => (Int -> f Int) -> Offset -> f Offset
seconds f (Offset secs) = secondsFromSecs fromSeconds f secs
{-# INLINE seconds #-}

-- | Lens for the minutes component of the 'Offset'
minutes :: Functor f => (Int -> f Int) -> Offset -> f Offset
minutes f (Offset secs) = minutesFromSecs fromSeconds f secs
{-# INLINE minutes #-}

-- | Lens for the hours component of the 'Offset'
hours :: Functor f => (Int -> f Int) -> Offset -> f Offset
hours f (Offset secs) = hoursFromSecs fromSeconds f secs
{-# INLINE hours #-}
