-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Interval
-- Copyright   :  (C) 2016 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  TBD
--
-- An 'Interval' is a period of time between two 'Instant's.
----------------------------------------------------------------------------
module Data.HodaTime.Interval
(
  -- * Types
  Interval
  -- * Constructor
  ,interval
  -- * Lenses
  ,start
  ,end
  -- * Functions
  ,contains
  ,duration
)
where

import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.Duration.Internal (Duration)
import Data.HodaTime.Instant (difference)

data Interval = Interval Instant Instant
    deriving (Eq, Ord, Show)    -- TODO: Remove Show

interval :: Instant -> Instant -> Interval
interval = Interval             -- TODO: We probably need some checks here

-- | Lens for the start component of the 'Interval'
start :: Functor f => (Instant -> f Instant) -> Interval -> f Interval
start f (Interval s e) = flip Interval e <$> f s
{-# INLINE start #-}

-- | Lens for the end component of the 'Interval'
end :: Functor f => (Instant -> f Instant) -> Interval -> f Interval
end f (Interval s e) = Interval s <$> f e
{-# INLINE end #-}

-- | Determines if the 'Instant' is between the start and end of the 'Interval'.  The interval includes the start but excludes the end
contains :: Interval -> Instant -> Bool
contains (Interval s e) i = s <= i && e > i

-- | Get the 'Duration' between the start and the end of the 'Interval'
duration :: Interval -> Duration
duration (Interval s e) = e `difference` s
