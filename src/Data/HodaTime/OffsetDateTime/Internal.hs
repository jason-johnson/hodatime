module Data.HodaTime.OffsetDateTime.Internal
(
   Offset(..)
  ,OffsetDateTime(..)
  ,empty
  ,toStringRep
)
where

import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.Constants (secondsPerHour)
import Data.Int (Int32)

-- | An 'Offset' from UTC in seconds.
newtype Offset = Offset { offsetSeconds :: Int32 }
  deriving (Eq, Ord, Show)     -- TODO: Remove Show

-- | An 'Offset' with an offset of 0.  This is equivalent to UTC
empty :: Offset
empty = Offset 0

-- TODO: temp solution until we deal with printing
toStringRep :: Offset -> String
toStringRep (Offset secs) = rep
  where
    utc = "UTC"
    rep = if secs == 0 then utc else utc ++ sign ++ show h ++ ":" ++ show s
    sign = if secs < 0 then "-" else "+"
    h = secs `div` secondsPerHour
    s = secs - (h*secondsPerHour)

-- | A 'CalendarDateTime' with a UTC offset.  This is the format used by e.g. HTTP.  This type has a fixed 'TimeZone' with the name "UTC(+/-)offset".  If the offset is
-- empty, the name of the 'TimeZone' will be UTC
newtype OffsetDateTime cal = OffsetDateTime (ZonedDateTime cal)
  deriving (Eq, Show)    -- TODO: Remove Show
