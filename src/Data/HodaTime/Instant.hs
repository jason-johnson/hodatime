module Data.HodaTime.Instant
(
   add
  ,difference
  ,minus
  ,now
  ,withOffset
  ,inZone
  ,fromSecondsSinceUnixEpoch
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond, unixDaysOffset)
import Data.HodaTime.Types (Instant(..), Duration(..), Calendar(..), LocalDateTime(..), OffsetDateTime(..), Offset(..), TimeZone, ZonedDateTime(..))
import qualified Data.HodaTime.Duration.Internal as D
import qualified Data.HodaTime.LocalTime.Internal as LTI (fromInstant)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI (fromInstant)
import Control.Arrow ((>>>), first)

-- Math

-- TODO: Do we want to keep add and minus or just make it possible to add negative durations?

-- | Add a 'Duration' to an 'Instant' to get a future 'Instant'. NOTE: does not handle all negative durations, use 'minus'
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
difference (Instant ldays lsecs lnsecs) (Instant rdays rsecs rnsecs) = Duration $ Instant days' secs' nsecs
    where
        days = ldays - rdays
        (days', secs) = safeMinus lsecs rsecs secondsPerDay days
        (secs', nsecs) = safeMinus lnsecs rnsecs nsecsPerSecond secs
        safeMinus l r size big 
            | r > l = (pred big, l + size - r)
            | otherwise = (big, l - r)

-- | Subtract a 'Duration' from an 'Instant' to get an 'Instant' in the past.  NOTE: does not handle negative durations, use 'add'
minus :: Instant -> Duration -> Instant
minus linstant (Duration rinstant) = getInstant $ difference linstant rinstant

-- Conversion

withOffset :: Instant -> Offset -> Calendar -> OffsetDateTime
withOffset instant offset calendar = OffsetDateTime (LocalDateTime date time) offset
    where
        instant' = instant `add` (D.seconds . fromIntegral . offsetSeconds $ offset)
        time = LTI.fromInstant instant'
        date
            | calendar == Gregorian || calendar == Iso  = GI.fromInstant instant' calendar
            | otherwise                                 = undefined     -- TODO: Why does compiler think the first this isn't total without the otherwise?

fromSecondsSinceUnixEpoch :: Int -> Instant
fromSecondsSinceUnixEpoch s = Instant days (fromIntegral secs) 0
    where
        (days, secs) = flip divMod secondsPerDay >>> first (fromIntegral . (subtract unixDaysOffset)) $ s

inZone :: Instant -> TimeZone -> Calendar -> ZonedDateTime
inZone instant tz calendar = ZonedDateTime odt tz
    where
        odt = withOffset instant offset calendar
        offset = undefined                              -- TODO: When TimeZone module is implemented we can finish this (look at the olson time zone series from hackage, but we can't use it all)