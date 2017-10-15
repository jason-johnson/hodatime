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
  ,LTI.fromInstant  -- TODO:  REMOVE THIS!  This is only exported for testing, remove it immediately after fixing fromSecondsSinceUnixEpoch
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Instant.Clock (now)
import Data.HodaTime.TimeZone.Internal (TimeZone)
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import qualified Data.HodaTime.LocalTime.Internal as LTI (fromInstant, Hour)    -- Made to warn so we don't forget to remove this stuff

-- Conversion

-- | Convert 'Instant' to a 'ZonedDateTime' in the specified time zone.  The calendar must be derivable or specified in the type explicitly
inTimeZone :: Instant -> TimeZone -> ZonedDateTime cal
inTimeZone _instant _tz = undefined