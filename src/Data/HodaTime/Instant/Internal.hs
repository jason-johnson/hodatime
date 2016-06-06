module Data.HodaTime.Instant.Internal
(
   Instant(..)
  ,fromUnixGetTimeOfDay
)
where

import Data.Word (Word32)
import Data.Int (Int32)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerMicrosecond, unixDaysOffset)
import Control.Arrow ((>>>), first)

-- | Represents a point on a global time line.  An Instant has no concept of time zone or
--   calendar.  It is nothing more than the number of nanoseconds since epoch (1.March.2000)
data Instant = Instant { iDays :: Int32, iSecs :: Word32, iNsecs :: Word32 }                -- TODO: Would this be better with only days and Word64 Nanos?  See if the math is easier
    deriving (Eq, Ord, Show)    -- TODO: Remove Show

fromUnixGetTimeOfDay :: Int -> Word32 -> Instant
fromUnixGetTimeOfDay s ms = Instant days (fromIntegral secs) nsecs
  where
    (days, secs) = flip divMod secondsPerDay >>> first (fromIntegral . subtract unixDaysOffset) $ s
    nsecs = ms * nsecsPerMicrosecond
