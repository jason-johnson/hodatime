{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HodaTime.Instant.Platform
(
  now
)
where

import Data.HodaTime.Instant.Internal (fromUnixGetTimeOfDay, Instant)
  
import System.Win32.Time
  
{-# INLINE now #-}
now :: IO Instant
now = undefined