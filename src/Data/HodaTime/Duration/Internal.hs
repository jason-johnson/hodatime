module Data.HodaTime.Duration.Internal
(
   Duration(..)
  ,normalize
  ,seconds
)
where

import Data.HodaTime.Instant.Internal (Instant(..))
import Control.Arrow ((>>>), (***))
import Data.HodaTime.Constants (secondsPerDay)

-- | Represents a duration of time between instants.  It can be from days to nanoseconds,
--   but anything longer is not representable by a duration because e.g. Months are calendar
--   specific concepts.
newtype Duration = Duration { getInstant :: Instant }
    deriving (Show)             -- TODO: Remove Show

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