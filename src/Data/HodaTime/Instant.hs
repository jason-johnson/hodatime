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
  ,inUtc
  -- * Debug - to be removed
  ,LTI.fromInstant  -- TODO:  REMOVE THIS!  This is only exported for testing, remove it immediately after fixing fromSecondsSinceUnixEpoch
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Instant.Clock (now)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)
import Data.HodaTime.Duration.Internal (Duration(..))
import Data.HodaTime.OffsetDateTime.Internal(Offset(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import qualified Data.HodaTime.OffsetDateTime.Internal as Offset (empty)
import qualified Data.HodaTime.Duration.Internal as D
import qualified Data.HodaTime.LocalTime.Internal as LTI (fromInstant)

-- | Create an 'Instant' from an 'Int' that represents a Unix Epoch
fromSecondsSinceUnixEpoch :: Int -> Instant
fromSecondsSinceUnixEpoch s = fromUnixGetTimeOfDay s 0

-- | Add a 'Duration' to an 'Instant' to get a future 'Instant'. /NOTE: does not handle all negative durations, use 'minus'/
add :: Instant -> Duration -> Instant
add (Instant ldays lsecs lnsecs) (Duration (Instant rdays rsecs rnsecs)) = Instant days' secs'' nsecs'
    where
        days = ldays + rdays
        secs = lsecs + rsecs
        nsecs = lnsecs + rnsecs
        (secs', nsecs') = adjust secs nsecs nsecsPerSecond
        (days', secs'') = adjust days secs' secondsPerDay
        adjust big small size
            | small >= size = (succ big, small - size)
            | otherwise = (big, small)

-- | Get the difference between two instances
difference :: Instant -> Instant -> Duration
difference (Instant ldays lsecs lnsecs) (Instant rdays rsecs rnsecs) = Duration $ Instant days' (fromIntegral secs'') (fromIntegral nsecs')
    where
        days = ldays - rdays
        secs = (fromIntegral lsecs - fromIntegral rsecs) :: Int                   -- TODO: We should specify exactly what sizes we need here.  Keep in mind we can depend that secs and nsecs are never negative so
        nsecs = (fromIntegral lnsecs - fromIntegral rnsecs) :: Int                -- TODO: there is no worry that we get e.g. (-nsecsPerSecond - -nsecsPerSecond) causing us to have more than nsecsPerSecond.
        (secs', nsecs') = normalize nsecs secs nsecsPerSecond
        (days', secs'') = normalize secs' days secondsPerDay
        normalize x bigger size
            | x < 0 = (pred bigger, x + size)
            | otherwise = (bigger, x)

-- | Subtract a 'Duration' from an 'Instant' to get an 'Instant' in the past.  /NOTE: does not handle negative durations, use 'add'/
minus :: Instant -> Duration -> Instant
minus linstant (Duration rinstant) = getInstant $ difference linstant rinstant

-- Conversion

{-^}
-- | Create an 'OffsetDateTime' from this Instant and an Offset
withOffset :: Instant -> Offset -> Calendar -> OffsetDateTime
withOffset instant offset calendar = OffsetDateTime (LocalDateTime date time) offset          -- TODO: I'm not sure I like applying the offset on construction.  See if we can defer it
    where
        instant' = instant `add` (D.fromSeconds . fromIntegral . offsetSeconds $ offset)
        time = LTI.fromInstant instant'
        date
            | calendar == Gregorian || calendar == Iso  = GI.fromInstantInCalendar instant' calendar
            | otherwise                                 = undefined     -- TODO: Why does compiler think this isn't total without the otherwise?

-- | Convert 'Instant' Into a 'ZonedDateTime' based on the supplied 'TimeZone' and 'Calendar'
inZone :: Instant -> TimeZone -> Calendar -> ZonedDateTime
inZone instant UTCzone calendar = ZonedDateTime odt UTCzone
  where
    odt = withOffset instant Offset.empty calendar
inZone instant tzi@TimeZone { } calendar = ZonedDateTime odt tzi
    where
        odt = withOffset instant offset calendar
        offset
            | otherwise = undefined       -- TODO: When TimeZone module is implemented we can finish this (look at the olson time zone series from hackage, but we can't use it all)
-}

-- | Convert 'Instant' to a 'ZonedDateTime' in the UTC time zone, ISO calendar
inUtc :: Instant -> ZonedDateTime
inUtc instant = undefined
