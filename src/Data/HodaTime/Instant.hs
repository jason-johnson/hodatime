{-# LANGUAGE TypeFamilies #-}

module Data.HodaTime.Instant
(
   add
  ,difference
  ,minus
)
where

import Prelude hiding ((+), (-))
import qualified Prelude as P
import Data.HodaTime.Constants (secondsPerDay, nsecsPerSecond)
import Data.HodaTime.Types (Instant(..), Duration(..))

add :: Instant -> Duration -> Instant
add (Instant ldays lsecs lnsecs) (Duration (Instant rdays rsecs rnsecs)) = Instant days' secs'' nsecs'
    where
        days = ldays P.+ rdays
        secs = lsecs P.+ rsecs
        nsecs = lnsecs P.+ rnsecs
        (secs', nsecs') = adjust secs nsecs nsecsPerSecond
        (days', secs'') = adjust days secs' secondsPerDay
        adjust big small size
            | small >= size = (succ big, small P.- size)
            | otherwise = (big, small)

difference :: Instant -> Instant -> Duration
difference (Instant ldays lsecs lnsecs) (Instant rdays rsecs rnsecs) = Duration $ Instant days' secs' nsecs
    where
        days = ldays P.- rdays
        (days', secs) = safeMinus lsecs rsecs secondsPerDay days
        (secs', nsecs) = safeMinus lnsecs rnsecs nsecsPerSecond secs
        safeMinus l r size big 
            | r > l = (pred big, l P.+ size P.- r)
            | otherwise = (big, l P.- r)

minus :: Instant -> Duration -> Instant
minus linstant (Duration rinstant) = getInstant $ difference linstant rinstant