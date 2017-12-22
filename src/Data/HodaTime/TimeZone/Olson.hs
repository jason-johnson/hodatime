module Data.HodaTime.TimeZone.Olson
(
   getTransitions
  ,ParseException(..)
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.ParseTZ (parsePosixString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get (Get, getWord8, getWord32be, getInt32be, getInt64be, getByteString, runGetOrFail, skip, isEmpty)
import Data.Word (Word8)
import Control.Monad (unless, replicateM)
import Data.List (foldl')
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)
import Data.HodaTime.Instant.Internal (Instant, fromSecondsSinceUnixEpoch, add, minus, bigBang)
import Data.HodaTime.Duration.Internal (fromSeconds)
import Data.HodaTime.Calendar.Gregorian.Internal (instantToYearMonthDay)

data ParseException = ParseException String Int
  deriving (Typeable, Show)

instance Exception ParseException

data Header = Header String Char Int Int Int Int Int Int

data TransInfo = TransInfo { tiOffset :: Int, tiIsDst :: Bool, abbr :: String }
  deriving (Eq, Show)

reservedSectionSize :: Int
reservedSectionSize = 15

getTransitions :: MonadThrow m => L.ByteString -> m (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
getTransitions bs = case runGetOrFail getTransitions' bs of
  Left (_, consumed, msg) -> throwM $ ParseException msg (fromIntegral consumed)
  Right (_, _, xs) -> return xs
  where
    getTransitions' = do
      header@(Header magic version _ _ _ _ _ _) <- getHeader
      unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)
      (getInt, header'@(Header _ _ isGmtCount isStdCount _ _ typeCount _)) <- getCorrectHeader header
      unless
        (isGmtCount == isStdCount && isStdCount == typeCount)
        (fail $ "format issue: sizes don't match: ttisgmtcnt = " ++ show isGmtCount ++ ", ttisstdcnt = " ++ show isStdCount ++ ", ttypecnt = " ++ show typeCount)
      (transitions, indexes, tInfos, leapsM) <- getPayload getInt header'
      tzString <- getTZString version
      finished <- isEmpty
      unless finished $ fail "unprocessed data still in olson file"
      let (utcM, calDateM) = buildTransitionMaps (zip transitions indexes) tInfos tzString
      return (utcM, calDateM, leapsM)

-- Get combinators

getCh :: Get Char
getCh = fmap toChar getWord8
  where
    toChar = toEnum . fromIntegral

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

getInt8 :: Get Int
getInt8 = fmap fromIntegral getWord8

getUInt32 :: Get Int
getUInt32 = fmap fromIntegral getWord32be

getInt32 :: Get Int
getInt32 = fmap fromIntegral getInt32be

getInt64 :: Get Int
getInt64 = fmap fromIntegral getInt64be

getHeader :: Get Header
getHeader = do
  magic <- (toString . B.unpack) <$> getByteString 4
  version <- getCh
  skip reservedSectionSize
  [ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen] <- replicateM 6 getUInt32
  return $ Header magic version ttisgmtcnt ttisstdcnt leapcnt transcnt ttypecnt abbrlen

getLeapInfo :: Get Int -> Get (Instant, Int)
getLeapInfo getInt = do
  instant <- fromSecondsSinceUnixEpoch <$> getInt
  lOffset <- getInt32
  return (instant, lOffset)

getPayload :: Get Int -> Header -> Get ([Instant], [Int], [TransInfo], LeapsMap)
getPayload getInt (Header _ _ isGmtCount isStdCount leapCount transCount typeCount abbrLen) = do
  transitions <- replicateM transCount $ fromSecondsSinceUnixEpoch <$> getInt
  indexes <- replicateM transCount getInt8
  types <- replicateM typeCount $ (,,) <$> getInt32 <*> getBool <*> getInt8
  abbrs <- (toString . B.unpack) <$> getByteString abbrLen
  leaps <- replicateM leapCount $ getLeapInfo getInt
  skip $ isStdCount + isGmtCount
  let tInfos = mapTransitionInfos abbrs types
  let leapM = addLeapTransition bigBang 0 $ importLeaps leaps
  return (transitions, indexes, tInfos, leapM)
getCorrectHeader :: Header -> Get (Get Int, Header)
getCorrectHeader header@(Header _ version isGmtCount isStdCount leapCount transCount typeCount abbrLen)
  | version == '\NUL'                 = return (getInt32, header)
  | version == '1' || version == '2'  = skipOldDataAndGetHeader
  | otherwise                         = fail $ "unknown olson version: " ++ show version
  where
    skipOldDataAndGetHeader = do
      skip $ transCount * 4 + transCount + typeCount * 4 + typeCount + typeCount + abbrLen + leapCount * 4 * 2 + isStdCount + isGmtCount
      correctHeader <- getHeader
      return (getInt64, correctHeader)

getTZString :: Char -> Get (Maybe String)
getTZString version
  | version == '\NUL'                 = return Nothing
  | version == '1' || version == '2'  = getTZString'
  | otherwise                         = fail $ "impossible: unknown version in getTZString"
  where
    getTZString' = do
      nl <- getCh
      unless (nl == '\n') (fail $ "POSIX TZ string preceded by non-newline:" ++ show nl)
      posixTZ <- getWhileM (/= '\n')
      return . Just $ posixTZ
    getWhileM p = do
      ch <- getCh
      if p ch then do
        rest <- getWhileM p
        return $ ch : rest
      else return []

-- helper fucntions

mapTransitionInfos :: String -> [(Int, Bool, Int)] -> [TransInfo]
mapTransitionInfos abbrs = fmap toTI
  where
    toTI (gmt, isdst, offset) = TransInfo gmt isdst $ getAbbr offset abbrs
    getAbbr offset = takeWhile (/= '\NUL') . drop offset

buildTransitionMaps :: [(Instant, Int)] -> [TransInfo] -> Maybe String -> (UtcTransitionsMap, CalDateTransitionsMap)
buildTransitionMaps transAndIndexes tInfos tzString = addLastMapEntries tzString' lastEntry lastTI utcMap calDateMap
  where
    tzString' = fmap parsePosixString tzString
    mkTI t = TransitionInfo (tiOffset t) (tiIsDst t) (abbr t)
    defaultTI = mkTI . findDefaultTransInfo $ tInfos
    oneSecond = fromSeconds 1
    initialUtcTransitions = addUtcTransition bigBang (TInfo defaultTI) emptyUtcTransitions
    (utcMap, calDateMap, lastEntry, lastTI) = foldl' go (initialUtcTransitions, emptyCalDateTransitions, Smallest, defaultTI) transAndIndexes
    go (utcM, calDateM, prevEntry, prevTI) (tran, idx) = (utcM', calDateM', Entry localTran, tInfo')
      where
        utcM' = addUtcTransition tran (TInfo tInfo') utcM
        calDateM' = addCalDateTransition prevEntry before (TInfo prevTI) calDateM
        localTran = applyOffset (tiOffset tInfo) $ tran
        before = Entry . flip minus oneSecond . applyOffset (tiUtcOffset prevTI) $ tran
        tInfo = tInfos !! idx
        tInfo' = mkTI tInfo

addLastMapEntries :: Maybe TransExpressionOrInfo -> IntervalEntry Instant -> TransitionInfo -> UtcTransitionsMap -> CalDateTransitionsMap
                                -> (UtcTransitionsMap, CalDateTransitionsMap)
addLastMapEntries Nothing start ti utcMap calDateMap = (utcMap, addCalDateTransition start Largest (TInfo ti) calDateMap)
-- NOTE: If the tzString does not have a time zone specification then the way we process the rest of the file should be correct (TODO: check offset) so we can ignore the string
addLastMapEntries (Just (TInfo _)) start ti utcMap calDateMap = (utcMap, addCalDateTransition start Largest (TInfo ti) calDateMap)
addLastMapEntries (Just texpr@(TExp (TransitionExpressionInfo stdExpr _ stdTI _))) start ti utcMap calDateMap = (utcMap', calDateMap')
  where
    utcMap' = addUtcTransition end texpr utcMap
    calDateMap' = addCalDateTransition cdEnd Largest texpr $ addCalDateTransition start before (TInfo ti) calDateMap
    cdEnd = Entry . applyOffset (tiUtcOffset stdTI) $ end
    before = Entry . flip minus (fromSeconds 1) . applyOffset (tiUtcOffset ti) $ end
    end = yearExpressionToInstant (y + 1) stdExpr   -- TODO: if we ever find the gap too large, we could add a check here before incrementing the year
    y = case start of
      (Entry trans) -> let (yr, _, _) = instantToYearMonthDay trans in fromIntegral yr
      _             -> error "impossible: got non Entry for last valid transition"

applyOffset :: Int -> Instant -> Instant
applyOffset off i = apply i d
  where
    apply = if off < 0 then minus else add
    d = fromSeconds . abs $ off

findDefaultTransInfo :: [TransInfo] -> TransInfo
findDefaultTransInfo tis = go . filter ((== False) . tiIsDst) $ tis
  where
    go [] = head tis
    go (ti:_) = ti

toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)