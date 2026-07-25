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
--
-- === Technical discussion: why the components are read-only functions, not lenses
--
-- 'hours', 'minutes' and 'seconds' are plain functions, not lenses.  In this library a lens exists to /modify/ a
-- value, and there is no honest way to modify a single component of an 'Offset' in isolation, because an 'Offset' is
-- a single /signed/ count of seconds: the sign belongs to the whole value, not to any one component.
--
-- The subtle part is that /additive/ modification would actually work.  Under an increment the component's current
-- value cancels out, so @over minutes (+45)@ reduces to adding 45 minutes to the whole offset — it turns @-01:30@
-- into @-00:45@, exactly as you would hope.  But that also makes it /identical/ to @'addClamped' o ('fromMinutes'
-- 45)@: as a lens it would buy nothing over the arithmetic that is already here.
--
-- What a lens /would/ add over those functions is precisely the part that is not well defined for a signed value:
--
--   * Reading a component in isolation (@view minutes@ of @-01:30@): is the minutes part @30@ or @-30@?  We answer
--     that for reads by making the accessors /sign-consistent/ — each component carries the offset's sign, so
--     @-01:30@ gives @-1@ hours and @-30@ minutes and @hours*3600 + minutes*60 + seconds@ always reconstructs the
--     total.  That is a getter, hence a function.
--   * An /absolute/ set (@set minutes 45@) or a non-additive change (@over minutes (*2)@): here the old value does
--     /not/ cancel, and the minutes slot of a negative offset has no canonical meaning, so there is no honest
--     implementation to offer.
--
-- We also considered a single lens over the whole value (its total seconds).  It is coherent, but redundant: an
-- 'Offset' is already just a signed scalar, so 'fromSeconds', 'fromMinutes' and 'fromHours' construct one and
-- 'addClamped' \/ 'minusClamped' do the arithmetic; modifying an 'Offset' embedded in a larger structure is done by
-- using those inside that structure's own modify, so a value lens would not compose any better.  It would only save
-- constructing a throwaway 'Offset' for a bit of math — not enough to justify a second way to do the same thing.
--
-- So: read a component with the functions here; build or adjust an 'Offset' as a whole with the constructors and
-- 'addClamped' \/ 'minusClamped'.  (Display is the job of "Data.HodaTime.Pattern", not of these accessors.)
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
  -- | Read-only functions (not lenses); see the /Technical discussion/ in the module header for why.
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

-- Accessors (read-only functions, not lenses; see the Technical discussion in the module header for the full
-- rationale).  Implementation note: these are sign-consistent -- each component carries the offset's sign, computed
-- with truncate-toward-zero `quot`/`rem`, NOT the floor `div`/`mod` used by the shared LocalTime helper (which is
-- correct there because a LocalTime's seconds are never negative, but would give the wrong split for a negative
-- offset, e.g. -01:30 -> hours -2, minutes +30).

-- | The seconds component of the 'Offset' (carries the sign; e.g. @-1@ for a @-00:00:01@ offset).
seconds :: Offset -> Int
seconds (Offset secs) = secs `rem` secondsPerMinute

-- | The minutes component of the 'Offset' (carries the sign; e.g. @-30@ for a @-01:30@ offset).
minutes :: Offset -> Int
minutes (Offset secs) = secs `rem` secondsPerHour `quot` secondsPerMinute

-- | The hours component of the 'Offset' (carries the sign; e.g. @-1@ for a @-01:30@ offset).
hours :: Offset -> Int
hours (Offset secs) = secs `quot` secondsPerHour
