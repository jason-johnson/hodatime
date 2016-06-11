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
import Data.HodaTime.Constants (secondsPerDay, secondsPerHour, nsecsPerSecond)

-- | Duration of standard weeks (a standard week is assumed to be exactly 7 24 hour days)
fromStandardWeeks :: Int -> Duration
fromStandardWeeks w = fromStandardDays $ w * 7

-- | Duration of standard days (a standard day is assumed to be exactly 24 hours)
fromStandardDays :: Int -> Duration
fromStandardDays d = Duration $ Instant (fromIntegral d) 0 0

-- | Duration of hours
fromHours :: Int -> Duration
fromHours = fromSeconds . (* secondsPerHour)

-- | Duration of minutes
fromMinutes :: Int -> Duration
fromMinutes = fromSeconds . (* 60)

-- | Duration of milliseconds
fromMilliseconds :: Int -> Duration
fromMilliseconds = fromNanoseconds . (* 1000000)

-- | Duration of microseconds
fromMicroseconds :: Int -> Duration
fromMicroseconds = fromNanoseconds . (* 1000)

-- | Duration of nanoseconds
fromNanoseconds :: Int -> Duration
fromNanoseconds ns = Duration $ Instant d s' (fromIntegral ns')
    where
        (s, ns') = normalize ns nsecsPerSecond
        (d, s') = normalize s secondsPerDay

-- | Add two durations together
add :: Duration -> Duration -> Duration
add (Duration instant) = Duration . I.add instant

-- | Subtract one duration from the other
minus :: Duration -> Duration -> Duration
minus (Duration linstant) (Duration rinstant) = difference linstant rinstant
