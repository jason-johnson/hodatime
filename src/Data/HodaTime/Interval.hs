module Data.HodaTime.Interval
(
   Interval(..)
  ,contains
  ,duration
)
where

import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.Duration.Internal (Duration)
import Data.HodaTime.Instant (difference)

data Interval = Interval { iStart :: Instant, iEnd :: Instant }
    deriving (Eq, Ord, Show)    -- TODO: Remove Show

-- | Determines if the 'Instant' is between the start and end of the 'Interval'.  The interval includes the start but excludes the end
contains :: Interval -> Instant -> Bool
contains (Interval s e) i = s <= i && e > i

-- | Get the 'Duration' between the start and the end of the 'Interval'
duration :: Interval -> Duration
duration (Interval s e) = e `difference` s
