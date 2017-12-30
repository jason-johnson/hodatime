-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Instant
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'Instant' is universal fixed moment in time.
----------------------------------------------------------------------------
module Data.HodaTime.Instant
(
  -- * Types
   Instant
  -- * Constructors
  ,fromSecondsSinceUnixEpoch
  ,now
  -- * Math
  ,add
  ,difference
  ,minus
  -- * Conversion
  ,inTimeZone
  -- * Debug - to be removed
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Instant.Platform (now)
import Data.HodaTime.TimeZone.Internal (TimeZone)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))

-- Conversion

-- | Convert 'Instant' to a 'ZonedDateTime' in the specified time zone.  The calendar must be derivable or specified in the type explicitly
inTimeZone :: Instant -> TimeZone -> ZonedDateTime cal
inTimeZone _instant _tz = undefined