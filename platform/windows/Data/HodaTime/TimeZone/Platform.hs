{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,fixedOffsetZone
  ,loadLocalZone
  ,loadTimeZone
  ,readReg
)
where

import Data.HodaTime.TimeZone.Internal
import Control.Exception (bracket)
import System.Win32.Types (LONG)
import System.Win32.Registry
import System.Win32.Time (SYSTEMTIME)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (sizeOf, Storable(..))
import Foreign.Ptr (castPtr)

data REG_TZI_FORMAT = REG_TZI_FORMAT
  {
     _tziBias :: LONG
    ,_tziStandardBias :: LONG
    ,_tziDaylightBias :: LONG
    ,_tziStandardDate :: SYSTEMTIME
    ,_tziDaylightDate :: SYSTEMTIME
  }
  deriving (Show,Eq,Ord)

instance Storable REG_TZI_FORMAT where
  sizeOf _ = sizeOf (undefined :: LONG) * 3 + sizeOf (undefined :: SYSTEMTIME) * 2
  alignment _ = 4

  poke buf (REG_TZI_FORMAT b sb db sd dd) = do
    pokeByteOff buf 0 b
    pokeByteOff buf 4 sb
    pokeByteOff buf 8 db
    pokeByteOff buf 12 sd
    pokeByteOff buf(12 + sizeOf (undefined :: SYSTEMTIME)) dd

  peek buf = REG_TZI_FORMAT
    <$> peekByteOff buf 0
    <*> peekByteOff buf 4
    <*> peekByteOff buf 8
    <*> peekByteOff buf 12
    <*> peekByteOff buf (12 + sizeOf (undefined :: SYSTEMTIME))

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadUTC = undefined

fixedOffsetZone :: String -> Int -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, TransitionInfo)
fixedOffsetZone = undefined

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String)
loadLocalZone = undefined

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadTimeZone = undefined

readReg :: String -> IO REG_TZI_FORMAT
readReg zone =
  bracket op regCloseKey $ \key ->
  allocaBytes sz $ \ptr -> do
    rvt <- regQueryValueEx key "TZI" ptr sz
    putStrLn $ "sizeOf timezone = " ++ show sz
    verifyAndPeak rvt ptr
    where
      sz = sizeOf (undefined :: REG_TZI_FORMAT)
      op = regOpenKeyEx hKEY_LOCAL_MACHINE hive kEY_QUERY_VALUE
      hive = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\" ++ zone
      verifyAndPeak rvt ptr
        | rvt == rEG_BINARY = peek . castPtr $ ptr
        | otherwise         = error $ "registry corrupt: TZI variable was non-binary type: " ++ show rvt
  