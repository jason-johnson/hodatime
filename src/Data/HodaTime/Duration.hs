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
import Data.Word (Word32)
import Data.HodaTime.Constants (minutesPerDay, secondsPerDay)

-- | Duration of d days
days :: Int -> Duration
days d = Duration $ Instant (fromIntegral d) 0 0

normalize :: (Num b, Integral a) => a -> (a -> c) -> a -> (b, c)
normalize to f = flip divMod to >>> fromIntegral *** f

negNormalize :: (Num b, Integral a) => a -> (a -> c) -> a -> (b, c)
negNormalize to f = arrow . abs
    where
        arrow = flip divMod to >>> fromIntegral . negate . succ *** f . (+ to) . negate

-- | Duration of h hours
hours :: Int -> Duration
hours h = Duration $ Instant d h' 0
    where
        toSeconds x = fromIntegral $ x * 60 * 60
        (d, h')
            | h >= 24 = normalize 24 toSeconds h
            | h < 0 = negNormalize 24 toSeconds h
            | otherwise = (0, toSeconds h)

-- | Duration of m minutes
minutes :: Int -> Duration
minutes m = Duration $ Instant d m' 0
    where
        toSeconds x = fromIntegral $ x * 60
        (d, m')
            | m >= minutesPerDay = normalize minutesPerDay toSeconds m
            | m < 0 = negNormalize minutesPerDay toSeconds m
            | otherwise = (0, toSeconds m)

-- | Duration of s seconds
seconds :: Int -> Duration
seconds s = Duration $ Instant d (fromIntegral s') 0
    where
        (d, s')
            | s >= secondsPerDay = normalize secondsPerDay id s
            | s < 0 = negNormalize secondsPerDay id s
            | otherwise = (0, s)

-- TODO: from here down needs to be fixed

milliseconds :: Word32 -> Duration
milliseconds ms = Duration $ Instant 0 0 (ms * 1000000)

microseconds :: Word32 -> Duration
microseconds ms = Duration $ Instant 0 0 (ms * 1000)

nanoseconds :: Word32 -> Duration
nanoseconds ns = Duration $ Instant 0 0 ns

-- | Add two durations together
add :: Duration -> Duration -> Duration
add (Duration instant) = Duration . I.add instant

-- | Subtract one duration from the other
minus :: Duration -> Duration -> Duration
minus (Duration linstant) (Duration rinstant) = difference linstant rinstant