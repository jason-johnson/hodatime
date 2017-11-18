{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HodaTime.Instant.Unix
(
    now
)
where

#include <sys/time.h>

import Data.HodaTime.Instant.Internal (fromUnixGetTimeOfDay, Instant)

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable

-- | Create an 'Instant' from the current system time
now :: IO Instant
now = allocaBytes #{size struct timeval} $ \ ptv -> do
  throwErrnoIfMinus1_ "gettimeofday" $ gettimeofday ptv nullPtr
  CTime sec <- #{peek struct timeval, tv_sec} ptv
  CSUSeconds usec <- #{peek struct timeval, tv_usec} ptv
  return $ fromUnixGetTimeOfDay (fromIntegral sec) (fromIntegral usec)
{-# INLINE now #-}

foreign import ccall unsafe "time.h gettimeofday"
  gettimeofday :: Ptr () -> Ptr () -> IO CInt
