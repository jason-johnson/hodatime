{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HodaTime.Instant.Platform
(
  now
)
where

import Data.HodaTime.Instant.Internal (fromUnixGetTimeOfDay, Instant)
  
import qualified System.Win32.Time as Win32
  
{-# INLINE now #-}
now :: IO Instant
now = do
  Win32.FILETIME ft <- Win32.getSystemTimeAsFileTime
  let (sec, usec) = (ft - win32_epoch_adjust) `divMod` 10000000
  return $ fromUnixGetTimeOfDay (fromIntegral sec) (fromIntegral usec)
  where
    win32_epoch_adjust = 116444736000000000