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
    deriving (Eq, Show)             -- TODO: Remove Show

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
