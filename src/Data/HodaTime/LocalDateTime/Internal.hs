module Data.HodaTime.LocalDateTime.Internal
(
)
where

import Data.Int (Int8, Int16)
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.HodaTime.LocalTime.Internal (LocalTime(..))
import Data.HodaTime.Instant.Internal (Instant(..))

{-
-- TODO: Calendar is assumed to be the same (do we actually need this stuff?)
instance Ord LocalDate where
    compare a b = comparing ldYear a b <> comparing ldMonth a b <> comparing ldDay a b

-- | Represents a specific date and time within its calendar system.  NOTE: a LocalDateTime does
--   *not* represent a specific time on the global time line because e.g. "10.March.2006 4pm" is a different instant
--   in most time zones.  Convert it to a ZonedDateTime first if you wish to convert to an instant (or use a convenience
--   function).
data LocalDateTime = LocalDateTime { ldtDate :: LocalDate, ldtTime :: LocalTime }
    deriving (Eq, Ord, Show)    -- TODO: Remove Show
-}
