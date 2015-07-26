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

import Data.HodaTime.Types (Duration(..), Instant(..))
import Data.HodaTime.Instant (difference)
import qualified Data.HodaTime.Instant as I (add)
import Control.Arrow ((>>>), (***))
import Data.HodaTime.Constants (minutesPerDay, secondsPerDay, nsecsPerSecond, microsecondsPerSecond, millisecondsPerSecond)

-- | Duration of d days
days :: Int -> Duration
days d = Duration $ Instant (fromIntegral d) 0 0

normalize :: (Num c, Integral a) => a -> a -> (a -> b) -> (c, b)
normalize x size f
    | x >= size = pos x
    | x < 0 = neg x
    | otherwise = (0, f x)
    where
        pos = flip divMod size >>> fromIntegral *** f
        neg = negArrow . abs
        negArrow = flip divMod size >>> fromIntegral . negate . succ *** f . (+ size) . negate

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

-- | Duration of s seconds
seconds :: Int -> Duration
seconds s = Duration $ Instant d (fromIntegral s') 0
    where
        (d, s') = normalize s secondsPerDay id

-- TODO: from here down needs to be fixed

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