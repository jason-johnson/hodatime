module Data.HodaTime.Types
(
   LocalTime(..)
  ,LocalDate(..)
  ,LocalDateTime(..)
  ,OffsetDateTime(..)
  ,ZonedDateTime(..)
)
where

import Data.HodaTime.Calendar (Calendar(..))
import Data.Word (Word32, Word16)
import Data.Int (Int8, Int16)
import Data.Ord (comparing)
import Data.Monoid ((<>))

-- | Represents a specific time of day with no reference to any calendar, date or time zone.
data LocalTime = LocalTime { ltSecs :: Word16, ltNsecs :: Word32 }
    deriving (Eq, Ord)

-- | Represents a specific date within its calendar system, with no reference to any time zone or time of day.
data LocalDate = LocalDate { ldYear :: Int16, ldMonth :: Int8, ldDay :: Int8, ldCalendar :: Calendar }
    deriving (Eq)

-- TODO: Calendar is assumed to be the same (do we actually need this stuff?)
instance Ord LocalDate where
    compare a b = comparing ldYear a b <> comparing ldMonth a b <> comparing ldDay a b

-- | Represents a specific date and time within its calendar system.  NOTE: a LocalDateTime does
--   *not* represent a specific time on the global time line because e.g. "10.March.2006 4pm" is a different instant
--   in most time zones.  Convert it to a ZonedDateTime first if you wish to convert to an instant (or use a convenience
--   function).
data LocalDateTime = LocalDateTime { ldtDate :: LocalDate, ldtTime :: LocalTime }
    deriving (Eq, Ord)

-- | An Offset from UTC in seconds.
newtype Offset = Offset { offsetSeconds :: Word32 }        -- TODO: Do we need this?  If OffsetDateTime is all that uses it, then we probably don't

-- | A LocalDateTime with a UTC offset.  This is the format used by e.g. HTTP.
data OffsetDateTime = OffsetDateTime { osdtDateTime :: LocalDateTime, osdtOffset :: Offset }

data TimeZone = TimeZone { }

-- | A LocalDateTime in a specific time zone. A ZonedDateTime is global and maps directly to a single Instant.
data ZonedDateTime = ZonedDateTime { zdtOffsetDateTime :: OffsetDateTime, zdtTimeZone :: TimeZone }