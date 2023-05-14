module Data.HodaTime.Duration.Internal
(
   Duration(..)
  ,normalize
  ,fromSeconds
  ,fromNanoseconds
)
where

import Data.HodaTime.Instant.Internal (Instant(..), Duration(..))
import Control.Arrow ((>>>), (***), first)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)

normalize :: Int -> Int -> (Int, Int)
normalize x size
    | x >= size = pos x
    | x < 0 = neg x
    | otherwise = (0, x)
    where
        split = flip divMod size
        pos = split >>> first fromIntegral
        neg = negArrow . abs
        negAdjust = fromIntegral . negate . succ *** (+ size) . negate
        negArrow x' = let (b,s) = split x' in
          if s == 0 then (negate b,s)     -- In the case that x' splits exactly we don't need to adjust further
          else negAdjust (b,s)

-- | Duration of seconds
fromSeconds :: Int -> Duration
fromSeconds s = Duration $ Instant (fromIntegral d) (fromIntegral s') 0
    where
        (d, s') = normalize s secondsPerDay

-- | Duration of nanoseconds
fromNanoseconds :: Int -> Duration
fromNanoseconds ns = Duration $ Instant (fromIntegral d) (fromIntegral s') (fromIntegral ns')
    where
        (s, ns') = normalize ns nsecsPerSecond
        (d, s') = normalize s secondsPerDay