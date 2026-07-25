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
  -- * Accessors
  --
  -- | Read-only.  See the note by their definitions for why they are functions rather than lenses.
  ,seconds
  ,minutes
  ,hours
  -- * Math
  ,addClamped
  ,minusClamped
)
where

import Data.HodaTime.Offset.Internal
import Data.HodaTime.Internal (secondsFromMinutes, secondsFromHours, clamp)
import Data.HodaTime.Constants (secondsPerHour, secondsPerMinute)

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

-- Accessors
--
-- NOTE: these are read-only functions, NOT lenses.  In this library a lens is for modification, and there is no
-- coherent way to 'set' a single component of a signed quantity: a positive @minutes@ on a negative 'Offset' (or
-- vice versa) has no sensible meaning, and any "set one component" would have to borrow from / flip the sign of the
-- others.  An 'Offset' is therefore only ever built or adjusted as a whole, via 'fromSeconds' \/ 'fromMinutes' \/
-- 'fromHours' \/ 'addClamped'.  Display, likewise, is the job of Data.HodaTime.Pattern, not of these accessors.
--
-- The components are sign-consistent: each one carries the offset's sign (using truncate-toward-zero 'quot'\/'rem',
-- not the floor 'div'\/'mod'), so e.g. a -01:30 offset gives hours -1 and minutes -30, and in general
-- hours o * 3600 + minutes o * 60 + seconds o == the offset's total seconds.

-- | The seconds component of the 'Offset' (carries the sign; e.g. @-1@ for a @-00:00:01@ offset).
seconds :: Offset -> Int
seconds (Offset secs) = secs `rem` secondsPerMinute

-- | The minutes component of the 'Offset' (carries the sign; e.g. @-30@ for a @-01:30@ offset).
minutes :: Offset -> Int
minutes (Offset secs) = secs `rem` secondsPerHour `quot` secondsPerMinute

-- | The hours component of the 'Offset' (carries the sign; e.g. @-1@ for a @-01:30@ offset).
hours :: Offset -> Int
hours (Offset secs) = secs `quot` secondsPerHour
