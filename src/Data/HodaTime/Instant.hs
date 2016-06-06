module Data.HodaTime.Instant
(
   add
  ,difference
  ,minus
  ,now
  ,withOffset
  ,inZone
  ,inUtc
  ,fromSecondsSinceUnixEpoch
)
where

import Data.HodaTime.Instant.Internal
import Data.HodaTime.Instant.Clock (now)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)
import Data.HodaTime.Duration.Internal (Duration(..))
import Data.HodaTime.OffsetDateTime.Internal(Offset(..), OffsetDateTime(..))
import Data.HodaTime.LocalDateTime.Internal (LocalDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.Calendar (Calendar(..))
import qualified Data.HodaTime.OffsetDateTime.Internal as Offset (empty)
import qualified Data.HodaTime.Duration.Internal as D
import qualified Data.HodaTime.LocalTime.Internal as LTI (fromInstant)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI (fromInstantInCalendar)

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

-- | Create an 'OffsetDateTime' from this Instant and an Offset
withOffset :: Instant -> Offset -> Calendar -> OffsetDateTime
withOffset instant offset calendar = OffsetDateTime (LocalDateTime date time) offset          -- TODO: I'm not sure I like applying the offset on construction.  See if we can defer it
    where
        instant' = instant `add` (D.seconds . fromIntegral . offsetSeconds $ offset)
        time = LTI.fromInstant instant'
        date
            | calendar == Gregorian || calendar == Iso  = GI.fromInstantInCalendar instant' calendar
            | otherwise                                 = undefined     -- TODO: Why does compiler think this isn't total without the otherwise?

fromSecondsSinceUnixEpoch :: Int -> Instant
fromSecondsSinceUnixEpoch s = fromUnixGetTimeOfDay s 0

inZone :: Instant -> TimeZone -> Calendar -> ZonedDateTime
inZone instant UTCzone calendar = ZonedDateTime odt UTCzone
  where
    odt = withOffset instant Offset.empty calendar
inZone instant tzi@TimeZone { } calendar = ZonedDateTime odt tzi
    where
        odt = withOffset instant offset calendar
        offset
            | otherwise = undefined       -- TODO: When TimeZone module is implemented we can finish this (look at the olson time zone series from hackage, but we can't use it all)

inUtc :: Instant -> ZonedDateTime
inUtc instant = undefined
