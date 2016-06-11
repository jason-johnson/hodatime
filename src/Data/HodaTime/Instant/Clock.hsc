{-# LANGUAGE ForeignFunctionInterface #-}

#ifndef mingw32_HOST_OS
#include <sys/time.h>
#endif

module Data.HodaTime.Instant.Clock
(
    now
)
where

import Data.HodaTime.Instant.Internal (fromUnixGetTimeOfDay, Instant)

#ifdef mingw32_HOST_OS
import System.Win32.Time
#else
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
#endif

{-# INLINE now #-}
now :: IO Instant
#ifdef mingw32_HOST_OS

#else

-- | Create an 'Instant' from the current system time
now = allocaBytes #{size struct timeval} $ \ ptv -> do
  throwErrnoIfMinus1_ "gettimeofday" $ gettimeofday ptv nullPtr
  CTime sec <- #{peek struct timeval, tv_sec} ptv
  CSUSeconds usec <- #{peek struct timeval, tv_usec} ptv
  return $ fromUnixGetTimeOfDay (fromIntegral sec) (fromIntegral usec)

foreign import ccall unsafe "time.h gettimeofday"
  gettimeofday :: Ptr () -> Ptr () -> IO CInt

#endif
