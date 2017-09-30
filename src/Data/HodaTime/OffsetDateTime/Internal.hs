module Data.HodaTime.OffsetDateTime.Internal
(
   Offset(..)
  ,OffsetDateTime(..)
  ,empty
)
where

import Data.HodaTime.CalendarDateTime.Internal (CalendarDate(..))
import Data.Int (Int32)
import Data.Word (Word64)

-- | An 'Offset' from UTC in seconds.
newtype Offset = Offset { offsetSeconds :: Int32 }
  deriving (Eq, Ord, Show)     -- TODO: Remove Show

empty :: Offset
empty = Offset 0

-- | A 'CalendarDateTime' with a UTC offset.  This is the format used by e.g. HTTP.
data OffsetDateTime cal = OffsetDateTime { osdtDate :: CalendarDate cal, osdtOffset :: Offset, osdtNanos :: Word64 }
  deriving (Eq, Ord, Show)    -- TODO: Remove Show
