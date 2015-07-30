module Data.HodaTime.Instant
(
   add
  ,difference
  ,minus
  ,withOffset
)
where

import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)
import Data.HodaTime.Types (Instant(..), Duration(..), Calendar(..), LocalDateTime(..), OffsetDateTime(..), Offset(..))
import qualified Data.HodaTime.Duration.Internal as D
import qualified Data.HodaTime.LocalTime.Internal as LTI (fromInstant)
import qualified Data.HodaTime.Calendar.Gregorian.Internal as GI (fromInstant)

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
            | calendar == Gregorian = GI.fromInstant instant'
            | calendar == Iso       = GI.fromInstant instant'
            | otherwise             = undefined     -- TODO: Why does compiler think the first this isn't total without the otherwise?