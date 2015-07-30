module Data.HodaTime.Duration
(
   days
  ,hours
  ,minutes
  ,seconds
  ,milliseconds
  ,microseconds
  ,nanoseconds
  ,add
  ,minus
)
where

import Data.HodaTime.Duration.Internal
import Data.HodaTime.Types (Duration(..), Instant(..))
import Data.HodaTime.Instant (difference)
import qualified Data.HodaTime.Instant as I (add)
import Data.HodaTime.Constants (minutesPerDay, millisecondsPerSecond, secondsPerDay, microsecondsPerSecond, nsecsPerSecond)

-- | Duration of d days
days :: Int -> Duration
days d = Duration $ Instant (fromIntegral d) 0 0

-- | Duration of h hours
hours :: Int -> Duration
hours h = Duration $ Instant d h' 0
    where
        toSeconds x = fromIntegral $ x * 60 * 60
        (d, h') = normalize h 24 toSeconds

-- | Duration of m minutes
minutes :: Int -> Duration
minutes m = Duration $ Instant d m' 0
    where
        toSeconds x = fromIntegral $ x * 60
        (d, m') = normalize m minutesPerDay toSeconds

-- | Duration of ms milliseconds
milliseconds :: Int -> Duration
milliseconds ms = Duration $ Instant d s' ns     -- TODO: Can day be affected here?  If so maybe there should be a check in the pattern below
    where
        toNanoseconds x = fromIntegral $ x * 1000000
        (s, ns) = normalize ms millisecondsPerSecond toNanoseconds
        (d, s') = normalize s secondsPerDay id

-- | Duration of ms microseconds
microseconds :: Int -> Duration
microseconds ms = Duration $ Instant d s' ns
    where
        toNanoseconds x = fromIntegral $ x * 1000
        (s, ns) = normalize ms microsecondsPerSecond toNanoseconds
        (d, s') = normalize s secondsPerDay id

-- | Duration of ns nanoseconds
nanoseconds :: Int -> Duration
nanoseconds ns = Duration $ Instant d s' (fromIntegral ns')
    where
        (s, ns') = normalize ns nsecsPerSecond id
        (d, s') = normalize s secondsPerDay id

-- | Add two durations together
add :: Duration -> Duration -> Duration
add (Duration instant) = Duration . I.add instant

-- | Subtract one duration from the other
minus :: Duration -> Duration -> Duration
minus (Duration linstant) (Duration rinstant) = difference linstant rinstant