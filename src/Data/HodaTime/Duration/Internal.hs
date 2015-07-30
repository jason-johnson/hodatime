module Data.HodaTime.Duration.Internal
(
   normalize
  ,seconds
)
where

import Data.HodaTime.Types (Duration(..), Instant(..))
import Control.Arrow ((>>>), (***))
import Data.HodaTime.Constants (secondsPerDay)

normalize :: (Num c, Integral a) => a -> a -> (a -> b) -> (c, b)
normalize x size f
    | x >= size = pos x
    | x < 0 = neg x
    | otherwise = (0, f x)
    where
        pos = flip divMod size >>> fromIntegral *** f
        neg = negArrow . abs
        negArrow = flip divMod size >>> fromIntegral . negate . succ *** f . (+ size) . negate

-- | Duration of s seconds
seconds :: Int -> Duration
seconds s = Duration $ Instant d (fromIntegral s') 0
    where
        (d, s') = normalize s secondsPerDay id