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
import Control.Exception (bracket)
import System.Win32.Types (LONG, HKEY)
import System.Win32.Registry
import System.Win32.Time (SYSTEMTIME(..))
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
  zone <- readLocalZoneName
  (utcM, calDateM) <- loadTimeZone zone
  return (utcM, calDateM, zone)

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadTimeZone "UTC" = return (utcM, calDateM)
  where
    (utcM, calDateM, _) = fixedOffsetZone "UTC" (Offset 0)
loadTimeZone zone = do
  (stdAbbr, dstAbbr, tzi) <- readTziForZone zone
  dynTzis <- readDynamicDstForZone zone
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
  regQueryValue key "TimeZoneKeyName"
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
    std <- regQueryValue key "Std"
    dst <- regQueryValue key "Dlt"
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