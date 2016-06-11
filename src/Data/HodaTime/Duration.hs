-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Duration
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- A 'Duration' is fixed period of time between global times.
----------------------------------------------------------------------------
module Data.HodaTime.Duration
(
  -- * Types
   Duration
  -- * Constructors
  ,fromStandardWeeks
  ,fromStandardDays
  ,fromHours
  ,fromMinutes
  ,fromSeconds
  ,fromMilliseconds
  ,fromMicroseconds
  ,fromNanoseconds
  -- * Math
  ,add
  ,minus
)
where

import Data.HodaTime.Duration.Internal
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Instant (difference)
import qualified Data.HodaTime.Instant as I (add)
import Data.HodaTime.Constants (minutesPerDay, millisecondsPerSecond, secondsPerDay, microsecondsPerSecond, nsecsPerSecond)

-- | Duration of w standard weeks (a standard week is assumed to be exactly 7 24 hour days)
fromStandardWeeks :: Int -> Duration
fromStandardWeeks w = fromStandardDays $ w * 7

-- | Duration of d standard days (a standard day is assumed to be exactly 24 hours)
fromStandardDays :: Int -> Duration
fromStandardDays d = Duration $ Instant (fromIntegral d) 0 0

-- | Duration of h hours
fromHours :: Int -> Duration
fromHours h = Duration $ Instant d h' 0
    where
        toSeconds x = fromIntegral $ x * 60 * 60
        (d, h') = normalize h 24 toSeconds

-- | Duration of m minutes
fromMinutes :: Int -> Duration
fromMinutes m = Duration $ Instant d m' 0
    where
        toSeconds x = fromIntegral $ x * 60
        (d, m') = normalize m minutesPerDay toSeconds

-- | Duration of ms milliseconds
fromMilliseconds :: Int -> Duration
fromMilliseconds ms = Duration $ Instant d s' ns     -- TODO: Can day be affected here?  If so maybe there should be a check in the pattern below
    where
        toNanoseconds x = fromIntegral $ x * 1000000
        (s, ns) = normalize ms millisecondsPerSecond toNanoseconds
        (d, s') = normalize s secondsPerDay id

-- | Duration of ms microseconds
fromMicroseconds :: Int -> Duration
fromMicroseconds ms = Duration $ Instant d s' ns
    where
        toNanoseconds x = fromIntegral $ x * 1000
        (s, ns) = normalize ms microsecondsPerSecond toNanoseconds
        (d, s') = normalize s secondsPerDay id

-- | Duration of ns nanoseconds
fromNanoseconds :: Int -> Duration
fromNanoseconds ns = Duration $ Instant d s' (fromIntegral ns')
    where
        (s, ns') = normalize ns nsecsPerSecond id
        (d, s') = normalize s secondsPerDay id

-- | Add two durations together
add :: Duration -> Duration -> Duration
add (Duration instant) = Duration . I.add instant

-- | Subtract one duration from the other
minus :: Duration -> Duration -> Duration
minus (Duration linstant) (Duration rinstant) = difference linstant rinstant
