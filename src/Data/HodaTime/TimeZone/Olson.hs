module Data.HodaTime.TimeZone.Olson
(
   getTransitions
  ,isOlsonFile
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
import Data.HodaTime.Instant.Internal (Instant(..), fromSecondsSinceUnixEpoch, minus, bigBang)
import Data.HodaTime.Duration.Internal (fromNanoseconds)
import Data.HodaTime.Offset.Internal (Offset(..), adjustInstant)
import Data.HodaTime.Calendar.Gregorian.Internal (instantToYearMonthDay, yearMonthDayToDays)

data ParseException = ParseException String Int
  deriving (Typeable, Show)

instance Exception ParseException

data Header = Header String Char Int Int Int Int Int Int

reservedSectionSize :: Int
reservedSectionSize = 15

getTransitions :: MonadThrow m => L.ByteString -> m (UtcTransitionsMap, CalDateTransitionsMap)
getTransitions bs = case runGetOrFail getTransitions' bs of
  Left (_, consumed, msg) -> throwM $ ParseException msg (fromIntegral consumed)
  Right (_, _, xs) -> return xs
  where
    getTransitions' = do
      header@(Header magic version _ _ _ _ _ _) <- getHeader
      unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)
      (getInt, header'@(Header _ _ _ _ _ _ typeCount _)) <- getCorrectHeader header
      unless
        (typeCount >= 1)
        (fail $ "format issue: ttypecnt must be at least 1 but is " ++ show typeCount)
      (transitions, indexes, tInfos) <- getPayload getInt header'
      tzString <- getTZString version
      finished <- isEmpty
      unless finished $ fail "unprocessed data still in olson file"
      let (utcM, calDateM) = buildTransitionMaps (zip transitions indexes) tInfos tzString
      return (utcM, calDateM)

isOlsonFile :: L.ByteString -> Bool
isOlsonFile bs = case runGetOrFail getMagic bs of
  Left _ -> False         -- If we can't load it for any reason, we wouldn't be able to use it as a time zone
  Right (_, _, x) -> x
  where
    getMagic = do
      (Header magic _ _ _ _ _ _ _) <- getHeader
      return $ magic == "TZif"

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

getPayload :: Get Int -> Header -> Get ([Instant], [Int], [TransitionInfo])
getPayload getInt (Header _ _ isGmtCount isStdCount leapCount transCount typeCount abbrLen) = do
  transitions <- replicateM transCount $ fromSecondsSinceUnixEpoch <$> getInt
  indexes <- replicateM transCount getInt8
  types <- replicateM typeCount $ (,,) <$> getInt32 <*> getBool <*> getInt8
  abbrs <- (toString . B.unpack) <$> getByteString abbrLen
  skip $ leapCount + isStdCount + isGmtCount
  let tInfos = mapTransitionInfos abbrs types
  return (transitions, indexes, tInfos)
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

mapTransitionInfos :: String -> [(Int, Bool, Int)] -> [TransitionInfo]
mapTransitionInfos abbrs = fmap toTI
  where
    toTI (gmt, isdst, offset) = TransitionInfo (Offset gmt) isdst $ getAbbr offset abbrs
    getAbbr offset = takeWhile (/= '\NUL') . drop offset

buildTransitionMaps :: [(Instant, Int)] -> [TransitionInfo] -> Maybe String -> (UtcTransitionsMap, CalDateTransitionsMap)
buildTransitionMaps transAndIndexes tInfos tzString = (utcMap', calDateMap')
  where
    (utcMap', calDateMap') = addLastMapEntries tzString' lastEntry lastTI utcMap calDateMap
    tzString' = fmap parsePosixString tzString
    defaultTI = findDefaultTransInfo $ tInfos
    initialUtcTransitions = addUtcTransition bigBang defaultTI emptyUtcTransitions
    (utcMap, calDateMap, lastEntry, lastTI) = foldl' go (initialUtcTransitions, emptyCalDateTransitions, Smallest, defaultTI) transAndIndexes
    go (utcM, calDateM, prevEntry, prevTI) (tran, idx) = (utcM', calDateM', Entry localTran, tInfo)
      where
        utcM' = addUtcTransition tran tInfo utcM
        calDateM' = addCalDateTransition prevEntry before prevTI calDateM
        localTran = adjustInstant (tiUtcOffset tInfo) $ tran
        before = Entry . flip minus (fromNanoseconds 1) . adjustInstant (tiUtcOffset prevTI) $ tran
        tInfo = tInfos !! idx

addLastMapEntries :: Maybe (Either TransitionInfo TransitionExpressionInfo) -> IntervalEntry Instant -> TransitionInfo ->
                      UtcTransitionsMap -> CalDateTransitionsMap -> (UtcTransitionsMap, CalDateTransitionsMap)
addLastMapEntries Nothing start ti utcMap calDateMap = (utcMap, addCalDateTransition start Largest ti calDateMap)
-- NOTE: If the tzString does not have a time zone specification then the way we process the rest of the file should be correct (TODO: check offset) so we can ignore it
addLastMapEntries (Just (Left _)) start ti utcMap calDateMap = (utcMap, addCalDateTransition start Largest ti calDateMap)
addLastMapEntries (Just (Right texpr@(TransitionExpressionInfo _ _ stdTI _))) prevTran prevTI utcMap calDateMap = (utcMap', calDateMap'')
  where
    utcMap' = addUtcTransitionExpression exprStart texpr utcMap
    calDateMap' = addCalDateTransition prevTran before prevTI calDateMap
    calDateMap'' = addCalDateTransitionExpression (Entry exprStart) Largest texpr calDateMap'
    before = Entry . flip minus (fromNanoseconds 1) $ exprStart
    exprStart = adjustInstant (tiUtcOffset stdTI) $ Instant yearStart 0 0  -- time changes are usually once per year
    yearStart = fromIntegral $ yearMonthDayToDays (y+1) (toEnum 0) 1
    y = case prevTran of
      (Entry trans) -> let (yr, _, _) = instantToYearMonthDay trans in fromIntegral yr
      _             -> error "impossible: got non Entry for last valid transition"

findDefaultTransInfo :: [TransitionInfo] -> TransitionInfo
findDefaultTransInfo tis = go . filter ((== False) . tiIsDst) $ tis
  where
    go [] = head tis
    go (ti:_) = ti

toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)