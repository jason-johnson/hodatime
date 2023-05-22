module Data.HodaTime.Instant.Internal
(
   Instant(..)
  ,Duration(..)
  ,fromUnixGetTimeOfDay
  ,fromSecondsSinceUnixEpoch
  ,add
  ,minus
  ,difference
  ,bigBang
)
where

import Data.Word (Word32)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond, nsecsPerMicrosecond, unixDaysOffset)
import Control.Arrow ((>>>), first)

-- types

-- | Represents a point on a global time line.  An Instant has no concept of time zone or
--   calendar.  It is nothing more than the number of nanoseconds since epoch (1.March.2000)
data Instant = Instant { iDays :: Int32, iSecs :: Word32, iNsecs :: Word32 }
  deriving (Eq, Ord)

-- | Represents a duration of time between instants.  It can be from days to nanoseconds,
--   but anything longer is not representable by a duration because e.g. Months are calendar
--   specific concepts.
newtype Duration = Duration { getInstant :: Instant } {- NOTE: Defined here to avoid circular dependancy with Duration.Internal -}
  deriving (Eq, Show)             -- TODO: Remove Show

instance Show Instant where
  show (Instant days secs nsecs) = intercalate "." [show (abs days), show secs, show nsecs, sign]
    where
      sign = if signum days == -1 then "BE" else "E"

-- interface

-- Smallest possible instant
bigBang :: Instant
bigBang = Instant minBound minBound minBound

-- | Create an 'Instant' from an 'Int' that represents a Unix Epoch
fromSecondsSinceUnixEpoch :: Int -> Instant
fromSecondsSinceUnixEpoch s = fromUnixGetTimeOfDay s 0

-- | Add a 'Duration' to an 'Instant' to get a future 'Instant'. /NOTE: does not handle all negative durations, use 'minus'/
add :: Instant -> Duration -> Instant
add (Instant ldays lsecs lnsecs) (Duration (Instant rdays rsecs rnsecs)) = Instant days' secs'' nsecs'
    where
        days = ldays + rdays
        secs = lsecs + rsecs
        nsecs = lnsecs + rnsecs
        (secs', nsecs') = adjust secs nsecs nsecsPerSecond
        (days', secs'') = adjust days secs' secondsPerDay
        adjust big small size
            | small >= size = (succ big, small - size)
            | otherwise = (big, small)

-- | Get the difference between two instances
difference :: Instant -> Instant -> Duration
difference (Instant ldays lsecs lnsecs) (Instant rdays rsecs rnsecs) = Duration $ Instant days' (fromIntegral secs'') (fromIntegral nsecs')
    where
        days = ldays - rdays
        secs = (fromIntegral lsecs - fromIntegral rsecs) :: Int                   -- TODO: We should specify exactly what sizes we need here.  Keep in mind we can depend that secs and nsecs are never negative so
        nsecs = (fromIntegral lnsecs - fromIntegral rnsecs) :: Int                -- TODO: there is no worry that we get e.g. (-nsecsPerSecond - -nsecsPerSecond) causing us to have more than nsecsPerSecond.
        (secs', nsecs') = normalize nsecs secs nsecsPerSecond
        (days', secs'') = normalize secs' days secondsPerDay
        normalize x bigger size
            | x < 0 = (pred bigger, x + size)
            | otherwise = (bigger, x)

-- | Subtract a 'Duration' from an 'Instant' to get an 'Instant' in the past.  /NOTE: does not handle negative durations, use 'add'/
minus :: Instant -> Duration -> Instant
minus linstant (Duration rinstant) = getInstant $ difference linstant rinstant

-- helper functions

fromUnixGetTimeOfDay :: Int -> Word32 -> Instant
fromUnixGetTimeOfDay s ms = Instant days (fromIntegral secs) nsecs
  where
    (days, secs) = flip divMod secondsPerDay >>> first (fromIntegral . subtract unixDaysOffset) $ s
    nsecs = ms * nsecsPerMicrosecond
