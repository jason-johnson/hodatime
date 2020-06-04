module Data.HodaTime.Offset.Internal
(
   Offset(..)
  ,maxOffsetHours
  ,maxOffsetSeconds
  ,minOffsetSeconds
  ,empty
  ,toStringRep
  ,fromSeconds
  ,adjustInstant
  ,addClamped
  ,minusClamped
)
where

import Data.HodaTime.Instant.Internal (Instant, add, minus)
import qualified Data.HodaTime.Duration.Internal as D (fromSeconds)
import Data.HodaTime.Constants (secondsPerHour)
import Data.HodaTime.Internal (secondsFromSeconds, clamp)

-- | An 'Offset' from UTC in seconds.
newtype Offset = Offset { offsetSeconds :: Int }  -- TODO: Any reason to make this 32 bit?  We don't need more space than 32 bit
  deriving (Eq, Ord, Show)     -- TODO: Remove Show

-- Offset specific constants

maxOffsetHours :: Num a => a
maxOffsetHours = 18

maxOffsetSeconds :: Num a => a
maxOffsetSeconds = maxOffsetHours * secondsPerHour

minOffsetSeconds :: Num a => a
minOffsetSeconds = negate maxOffsetSeconds

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

-- | Create an 'Offset' of (clamped) s seconds.
fromSeconds :: Integral a => a -> Offset
fromSeconds = Offset . secondsFromSeconds . clamp minOffsetSeconds maxOffsetSeconds

-- | Add one 'Offset' to another  /NOTE: if the result of the addition is outside the accepted range it will be clamped/
addClamped :: Offset -> Offset -> Offset
addClamped (Offset lsecs) (Offset rsecs) = fromSeconds $ lsecs + rsecs
  
-- | Subtract one 'Offset' to another.  /NOTE: See 'add' above/
minusClamped :: Offset -> Offset -> Offset
minusClamped (Offset lsecs) (Offset rsecs) = fromSeconds $ lsecs - rsecs

-- helper functions

adjustInstant :: Offset -> Instant -> Instant
adjustInstant (Offset secs) instant = instant'
  where
    op = if secs < 0 then minus else add
    duration = D.fromSeconds . abs $ secs
    instant' = instant `op` duration