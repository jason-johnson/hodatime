{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Instant
(
   Instant(..)
  ,Duration(..)  
)
where

import Prelude hiding ((+), (-))
import qualified Prelude as P
import Data.Word (Word32, Word16)
import Data.Int (Int32)
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)

-- | Represents a point on a global time line.  An Instant has no concept of time zone or
--   calendar.  It is nothing more than the number of nanoseconds since epoch (1.March.2000)
data Instant = Instant { iDays :: Int32, iSecs :: Word16, iNsecs :: Word32 }                -- TODO: Would this be better with only days and Word64 Nanos?  See if the math is easier
    deriving (Eq, Ord, Show)    -- TODO: Remove Show

newtype Duration = Duration Instant
    deriving (Show)             -- TODO: Remove Show

class Torsor a where
    type Diff a
    (+) :: a -> Diff a -> a
    (-) :: a -> a -> Diff a

addDuration :: Instant -> Duration -> Instant
addDuration (Instant ldays lsecs lnsecs) (Duration (Instant rdays rsecs rnsecs)) = Instant days' secs'' nsecs'
    where
        days = ldays P.+ rdays
        secs = lsecs P.+ rsecs
        nsecs = lnsecs P.+ rnsecs
        (secs', nsecs') = adjust secs nsecs nsecsPerSecond
        (days', secs'') = adjust days secs' secondsPerDay
        adjust big small size
            | small >= size = (succ big, small P.- size)
            | otherwise = (big, small)

diffInstance :: Instant -> Instant -> Duration
diffInstance (Instant ldays lsecs lnsecs) (Instant rdays rsecs rnsecs) = Duration $ Instant days' secs' nsecs
    where
        days = ldays P.- rdays
        (days', secs) = safeMinus lsecs rsecs secondsPerDay days
        (secs', nsecs) = safeMinus lnsecs rnsecs nsecsPerSecond secs
        safeMinus l r size big 
            | r > l = (pred big, l P.+ size P.- r)
            | otherwise = (big, l P.- r)

instance Torsor Instant where
    type Diff Instant = Duration
    (+) = addDuration
    (-) = diffInstance

instance Torsor Duration where
    type Diff Duration = Duration
    (+) (Duration instant) = Duration . addDuration instant
    (Duration linstant) - (Duration rinstant) = diffInstance linstant rinstant