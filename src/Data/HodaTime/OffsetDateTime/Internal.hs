module Data.HodaTime.OffsetDateTime.Internal
(
   Offset(..)
  ,empty
  ,OffsetDateTime(..)
)
where

import Data.Int (Int32)
import Data.HodaTime.LocalDateTime.Internal(LocalDateTime)

-- | An 'Offset' from UTC in seconds.
newtype Offset = Offset { offsetSeconds :: Int32 }
    deriving (Eq, Ord, Show)     -- TODO: Remove Show

empty :: Offset
empty = Offset 0

-- | A 'LocalDateTime' with a UTC offset.  This is the format used by e.g. HTTP.
data OffsetDateTime = OffsetDateTime { osdtDateTime :: LocalDateTime, osdtOffset :: Offset }
    deriving (Eq, Ord, Show)    -- TODO: Remove Show
