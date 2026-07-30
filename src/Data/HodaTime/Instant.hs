-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Instant
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- An 'Instant' is universal fixed moment in time.
----------------------------------------------------------------------------
module Data.HodaTime.Instant
(
  -- * Types
   Instant
  -- * Constructors
  ,fromSecondsSinceUnixEpoch
  -- | The current time as an 'Instant'.
  --
  --   Note: Hoda Time does not model leap seconds, so 'now' follows POSIX time semantics — the returned 'Instant'
  --   does not include leap seconds (every day is treated as exactly 86400 seconds).  This keeps @now@ consistent with
  --   the rest of the library: round-tripping through a 'ZonedDateTime' never adds or removes leap seconds.
  ,now
  -- * Math
  ,add
  ,difference
  ,minus
  -- * Conversion
  ,inTimeZone
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Instant.Platform (now)
import Data.HodaTime.TimeZone.Internal (TimeZone)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..), fromInstant)
import Data.HodaTime.CalendarDateTime.Internal (IsCalendarDateTime)

-- Conversion

-- | Convert 'Instant' to a 'ZonedDateTime' in the specified time zone.  The calendar must be derivable or specified in the type explicitly
inTimeZone :: IsCalendarDateTime cal => Instant -> TimeZone -> ZonedDateTime cal
inTimeZone instant tz = fromInstant instant tz