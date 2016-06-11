module Data.HodaTime.Duration.Internal
(
   Duration(..)
  ,normalize
  ,fromSeconds
)
where

import Data.HodaTime.Instant.Internal (Instant(..))
import Control.Arrow ((>>>), (***), first)
import Data.HodaTime.Constants (secondsPerDay)

-- | Represents a duration of time between instants.  It can be from days to nanoseconds,
--   but anything longer is not representable by a duration because e.g. Months are calendar
--   specific concepts.
newtype Duration = Duration { getInstant :: Instant }
    deriving (Show)             -- TODO: Remove Show

normalize :: (Num b, Integral a) => a -> a -> (b, a)
normalize x size
    | x >= size = pos x
    | x < 0 = neg x
    | otherwise = (0, x)
    where
        pos = flip divMod size >>> first fromIntegral
        neg = negArrow . abs
        negArrow = flip divMod size >>> fromIntegral . negate . succ *** (+ size) . negate

-- | Duration of seconds
fromSeconds :: Int -> Duration
fromSeconds s = Duration $ Instant d (fromIntegral s') 0
    where
        (d, s') = normalize s secondsPerDay
