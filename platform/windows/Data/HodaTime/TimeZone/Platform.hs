{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,loadLocalZone
  ,loadTimeZone
  ,loadAvailableZones
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.Instant.Internal (Instant(..), bigBang, minus)
import Data.HodaTime.Offset.Internal (Offset(..))
import Data.HodaTime.Duration.Internal (fromNanoseconds)
import Data.HodaTime.Calendar.Gregorian.Internal (yearMonthDayToDays)

import Data.Char (isDigit)
import Data.List (sortOn, foldl')
import Control.Monad (forM)
import Control.Exception (bracket, try, SomeException)
import Data.Int (Int32)
import System.Win32.Types (LONG, HKEY, peekTString, withTString)
import System.Win32.Registry
import System.Win32.Time (SYSTEMTIME(..))
import System.Win32.DLL (loadLibrary, getProcAddress)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (sizeOf, Storable(..))
import Foreign.Ptr (castPtr, castPtrToFunPtr, Ptr, FunPtr)
import Foreign.C.Types (CWchar)
import Foreign.C.String (CString, withCString)

-- ICU-based conversion between IANA and Windows zone ids.  Windows uses its own registry zone names (e.g.
-- @"W. Europe Standard Time"@) while the rest of the world uses IANA ids (e.g. @"Europe/Zurich"@).  Rather than
-- vendoring a CLDR windowsZones table (which Microsoft explicitly advises against, since time zones change often and
-- they maintain the data), we delegate to ICU, which has shipped with Windows since Windows 10 v1703 (build 15063)
-- and is kept current by Windows Update.  We load @icu.dll@ dynamically (there is no import library for the
-- system copy, so it cannot be linked against) and resolve the two conversion functions at runtime.  On systems
-- where @icu.dll@ or the symbols are unavailable (e.g. Windows before v1703) the conversions return 'Nothing' and we
-- fall back to treating names as Windows registry names, i.e. the pre-ICU behaviour.
type GetWindowsTimeZoneID = Ptr CWchar -> Int32 -> Ptr CWchar -> Int32 -> Ptr Int32 -> IO Int32
type GetTimeZoneIDForWindowsID = Ptr CWchar -> Int32 -> CString -> Ptr CWchar -> Int32 -> Ptr Int32 -> IO Int32

foreign import ccall unsafe "dynamic"
  mkGetWindowsTimeZoneID :: FunPtr GetWindowsTimeZoneID -> GetWindowsTimeZoneID

foreign import ccall unsafe "dynamic"
  mkGetTimeZoneIDForWindowsID :: FunPtr GetTimeZoneIDForWindowsID -> GetTimeZoneIDForWindowsID

-- | The two ICU zone-conversion functions, resolved once from @icu.dll@.  'Nothing' when ICU is unavailable.
{-# NOINLINE icuZoneFns #-}
icuZoneFns :: Maybe (GetWindowsTimeZoneID, GetTimeZoneIDForWindowsID)
icuZoneFns = unsafePerformIO $ do
  r <- try loadFns :: IO (Either SomeException (GetWindowsTimeZoneID, GetTimeZoneIDForWindowsID))
  return $ either (const Nothing) Just r
  where
    loadFns = do
      hmod <- loadLibrary "icu.dll"
      p1 <- getProcAddress hmod "ucal_getWindowsTimeZoneID"
      p2 <- getProcAddress hmod "ucal_getTimeZoneIDForWindowsID"
      return (mkGetWindowsTimeZoneID (castPtrToFunPtr p1), mkGetTimeZoneIDForWindowsID (castPtrToFunPtr p2))

-- | Output buffer size in UChar units for ICU zone-id conversions; zone ids are far shorter than this.
icuBufLen :: Int
icuBufLen = 128

-- | Translate an IANA zone id (e.g. @"Europe/Zurich"@) to its Windows registry zone name via ICU.  Returns 'Nothing'
--   when the argument is not a known IANA id (for instance it is already a Windows name) or ICU is unavailable.
ianaToWindowsZone :: String -> IO (Maybe String)
ianaToWindowsZone iana = case icuZoneFns of
  Nothing -> return Nothing
  Just (getWindowsTZ, _) ->
    withTString iana $ \pIana ->
    allocaArray icuBufLen $ \pOut ->
    alloca $ \pStatus -> do
      poke pStatus 0
      n <- getWindowsTZ pIana (-1) pOut (fromIntegral icuBufLen) pStatus
      st <- peek pStatus
      if st <= 0 && n > 0 then Just <$> peekTString pOut else return Nothing

-- | Translate a Windows registry zone name to its canonical IANA zone id (CLDR region @"001"@) via ICU.  Returns
--   'Nothing' when the argument is not a known Windows name or ICU is unavailable.
windowsZoneToIana :: String -> IO (Maybe String)
windowsZoneToIana win = case icuZoneFns of
  Nothing -> return Nothing
  Just (_, getTZForWindows) ->
    withTString win $ \pWin ->
    withCString "001" $ \pRegion ->
    allocaArray icuBufLen $ \pOut ->
    alloca $ \pStatus -> do
      poke pStatus 0
      n <- getTZForWindows pWin (-1) pRegion pOut (fromIntegral icuBufLen) pStatus
      st <- peek pStatus
      if st <= 0 && n > 0 then Just <$> peekTString pOut else return Nothing


-- | Accept either a Windows registry zone name or an IANA id.  If ICU recognises the argument as an IANA id we use
--   the matching Windows name; otherwise we assume it is already a Windows name (or ICU is unavailable).
resolveWindowsZone :: String -> IO String
resolveWindowsZone zone = maybe zone id <$> ianaToWindowsZone zone

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

  poke _ _ = error "poke not implemented"

  peek buf = REG_TZI_FORMAT
    <$> peekByteOff buf 0
    <*> peekByteOff buf 4
    <*> peekByteOff buf 8
    <*> peekByteOff buf 12
    <*> peekByteOff buf (12 + sizeOf (undefined :: SYSTEMTIME))

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap)
loadUTC = loadTimeZone "UTC"

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, String)
loadLocalZone = do
  winZone <- readLocalZoneName
  (utcM, calDateM) <- loadTimeZone winZone
  ianaName <- windowsZoneToIana winZone
  return (utcM, calDateM, maybe winZone id ianaName)

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadTimeZone "UTC" = return (utcM, calDateM)
  where
    (utcM, calDateM, _) = fixedOffsetZone "UTC" (Offset 0)
loadTimeZone zone = do
  winZone <- resolveWindowsZone zone
  (stdAbbr, dstAbbr, tzi) <- readTziForZone winZone
  dynTzis <- readDynamicDstForZone winZone
  return $ mkZoneMaps stdAbbr dstAbbr tzi dynTzis

loadAvailableZones :: IO [String]
loadAvailableZones = readAllZoneNames

-- conversion from Windows types

mkZoneMaps :: String -> String -> REG_TZI_FORMAT -> [(Int, REG_TZI_FORMAT)] -> (UtcTransitionsMap, CalDateTransitionsMap)
mkZoneMaps stdAbbr dstAbbr defaultTzi dynTzis = (utcMap, calDateMap')
  where
    dynTzis' = sortOn fst dynTzis
    getInitialExpr [] = tziToExprInfo defaultTzi
    getInitialExpr ((_, tzi):_) = tziToExprInfo tzi
    tl [] = []
    tl (_:xs) = xs
    initialExpr = getInitialExpr dynTzis'
    initialUtcM = addUtcTransitionExpression bigBang initialExpr emptyUtcTransitions
    calDateMap' = addCalDateTransitionExpression lastEntry Largest lastExpr calDateMap
    (utcMap, calDateMap, lastEntry, lastExpr) = foldl' go (initialUtcM, emptyCalDateTransitions, Smallest, initialExpr) $ tl dynTzis'
    go (utcM, calDateM, prevEntry, prevExpr) (y, tzi) = (utcM', calDateM', Entry tran, expr)
      where
        expr = tziToExprInfo tzi
        m = toEnum 0
        tran = Instant (fromIntegral $ yearMonthDayToDays y m 1) 0 0
        utcM' = addUtcTransitionExpression tran expr utcM
        calDateM' = addCalDateTransitionExpression prevEntry before prevExpr calDateM
        before = Entry . flip minus (fromNanoseconds 1) $ tran
    tziToExprInfo (REG_TZI_FORMAT bias stdBias dstBias end start) = TransitionExpressionInfo startExpr endExpr stdTI dstTI
      where
        startExpr = systemTimeToNthDayExpression start stdOffSecs
        endExpr = systemTimeToNthDayExpression end dstOffSecs
        stdTI = TransitionInfo (Offset stdOffSecs) False stdAbbr
        dstTI = TransitionInfo (Offset dstOffSecs) True dstAbbr
        stdOffSecs = 60 * (negate . fromIntegral $ bias + stdBias)
        dstOffSecs = stdOffSecs + 60 * (negate . fromIntegral $ dstBias)

-- TODO: When wYear (the first, discarded field) is non-zero the SYSTEMTIME is an absolute one-time transition, not a
--       recurring yearly pattern.  We currently always treat it as the recurring (NthDay) form, which mis-reads those
--       explicit transitions.  Handle the wYear /= 0 case as an explicit transition instead.
systemTimeToNthDayExpression :: SYSTEMTIME -> Int -> TransitionExpression
systemTimeToNthDayExpression (SYSTEMTIME _ m d nth h mm s _) offsetSecs = NthDayExpression (fromIntegral m - 1) (adjust . fromIntegral $ nth) (fromIntegral d) s''
  where
    adjust 5 = -1                     -- In the registry, 5 actually means last which is -1 for us
    adjust n = n - 1                  -- Switch start nth to zero based
    s'' = s' - offsetSecs             -- Windows times are always local, so convert back to UTC
    s' = h' + mm' + fromIntegral s
    h' = fromIntegral h * 60 * 60
    mm' = fromIntegral mm * 60

readLocalZoneName :: IO String
readLocalZoneName =
  bracket op regCloseKey $ \key ->
  regQueryValueString key "TimeZoneKeyName"
    where
      op = regOpenKeyEx hKEY_LOCAL_MACHINE hive kEY_QUERY_VALUE
      hive = "SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation"

readAllZoneNames :: IO [String]
readAllZoneNames =
  bracket op regCloseKey $ \key ->
  regEnumKeys key
    where
      op = regOpenKeyEx hKEY_LOCAL_MACHINE hive kEY_READ
      hive = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones"

readTziForZone :: String -> IO (String, String, REG_TZI_FORMAT)
readTziForZone zone =
  bracket op regCloseKey $ \key -> do
    std <- regQueryValueString key "Std"
    dst <- regQueryValueString key "Dlt"
    tzi <- readTzi key "TZI"
    return (std, dst, tzi)
    where
      op = regOpenKeyEx hKEY_LOCAL_MACHINE hive kEY_QUERY_VALUE
      hive = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\" ++ zone

readDynamicDstForZone :: String -> IO [(Int, REG_TZI_FORMAT)]
readDynamicDstForZone zone = do
  hasDyn <- hasDynDst
  if hasDyn
  then 
    bracket (op dynDstHive) regCloseKey $ \dstKey -> do
      vals <- regEnumKeyVals dstKey
      let years = foldr toYear [] vals
      forM years $ \y -> do
        tzi <- readTzi dstKey y
        return (read y, tzi)
  else return []
  where
    hasDynDst = bracket (op tzHive) regCloseKey $ \key -> do
                  dyn <- regEnumKeys key
                  return $ length dyn == 1 && dyn !! 0 == "Dynamic DST"
    toYear (y, _, _) xs | all isDigit y = y:xs
                        | otherwise = xs
    op hive = regOpenKeyEx hKEY_LOCAL_MACHINE hive kEY_READ
    tzHive = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\" ++ zone
    dynDstHive = tzHive ++ "\\Dynamic DST"

readTzi :: HKEY -> String -> IO REG_TZI_FORMAT
readTzi key p =
  allocaBytes sz $ \ptr -> do
    rvt <- regQueryValueEx key p ptr sz
    verifyAndPeak rvt ptr
  where
    sz = sizeOf (undefined :: REG_TZI_FORMAT)
    verifyAndPeak rvt ptr
        | rvt == rEG_BINARY = peek . castPtr $ ptr
        | otherwise         = error $ "registry corrupt: TZI variable was non-binary type: " ++ show rvt

-- | Read a named REG_SZ (or REG_EXPAND_SZ) value as a 'String'.  Note: 'regQueryValue' reads the /default/ value of a
--   subkey, not a named value, so named string values (Std, Dlt, TimeZoneKeyName) must go through 'regQueryValueEx'.
regQueryValueString :: HKEY -> String -> IO String
regQueryValueString key name =
  allocaBytes sz $ \ptr -> do
    rvt <- regQueryValueEx key name ptr sz
    if rvt == rEG_SZ || rvt == rEG_EXPAND_SZ
      then peekTString (castPtr ptr)
      else error $ "registry corrupt: " ++ name ++ " was non-string type: " ++ show rvt
  where
    sz = 512