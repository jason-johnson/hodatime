module Data.HodaTime.TimeZone.Olson
(
   getTransitions
  ,ParseException(..)
)
where

import Data.HodaTime.TimeZone.Internal

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

data ParseException = ParseException String Int
  deriving (Typeable, Show)

instance Exception ParseException

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
      (magic, _version, ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen) <- getHeader
      unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)
      unless
        (ttisgmtcnt == ttisstdcnt && ttisstdcnt == ttypecnt)
        (fail $ "format issue, sizes don't match: ttisgmtcnt = " ++ show ttisgmtcnt ++ ", ttisstdcnt = " ++ show ttisstdcnt ++ ", ttypecnt = " ++ show ttypecnt)
      (utcM, calDateM, leapsM) <- getPayload transcnt ttypecnt abbrlen leapcnt ttisstdcnt ttisgmtcnt
      finished <- isEmpty
      unless finished $ fail "unprocessed data still in olson file"
      return (utcM, calDateM, leapsM)

-- Get combinators

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

getHeader :: Get (String, Word8, Int, Int, Int, Int, Int, Int)
getHeader = do
  magic <- (toString . B.unpack) <$> getByteString 4
  version <- getWord8
  skip reservedSectionSize
  [ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen] <- replicateM 6 getUInt32
  return (magic, version, ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen)

getLeapInfo :: Get (Instant, Int)
getLeapInfo = do
  instant <- fromSecondsSinceUnixEpoch <$> getInt32
  lOffset <- getInt32
  return (instant, lOffset)

getPayload :: Int -> Int -> Int -> Int -> Int -> Int -> Get (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
getPayload transCount typeCount abbrLen leapCount isStdCount isGmtCount = do
  transitions <- replicateM transCount $ fromSecondsSinceUnixEpoch <$> getInt32
  indexes <- replicateM transCount getInt8
  types <- replicateM typeCount $ (,,) <$> getInt32 <*> getBool <*> getInt8
  abbrs <- (toString . B.unpack) <$> getByteString abbrLen
  leaps <- replicateM leapCount getLeapInfo
  skip $ isStdCount + isGmtCount
  let tInfos = mapTransitionInfos abbrs types
  let (utcM, calDateM) = buildTransitionMaps (zip transitions indexes) tInfos
  let leapM = addLeapTransition bigBang 0 $ importLeaps leaps
  return (utcM, calDateM, leapM)

-- helper fucntions

mapTransitionInfos :: String -> [(Int, Bool, Int)] -> [TransInfo]
mapTransitionInfos abbrs = fmap toTI
  where
    toTI (gmt, isdst, offset) = TransInfo gmt isdst $ getAbbr offset abbrs
    getAbbr offset = takeWhile (/= '\NUL') . drop offset

buildTransitionMaps :: [(Instant, Int)] -> [TransInfo] -> (UtcTransitionsMap, CalDateTransitionsMap)
buildTransitionMaps transAndIndexes tInfos = (utcMap, calDateMap')
  where
    calDateMap' = addCalDateTransition lastEntry Largest lastTI calDateMap -- TODO: At some point we may want to have a special POSIX tInfo for generating these from TZ string
    mkTI t = TransitionInfo (tiOffset t) (tiIsDst t) (abbr t)
    defaultTI = mkTI . findDefaultTransInfo $ tInfos
    oneSecond = fromSeconds 1
    initialUtcTransitions = addUtcTransition bigBang defaultTI emptyUtcTransitions
    (utcMap, calDateMap, lastEntry, lastTI) = foldl' go (initialUtcTransitions, emptyCalDateTransitions, Smallest, defaultTI) transAndIndexes
    go (utcM, calDateM, prevEntry, prevTI) (tran, idx) = (utcM', calDateM', Entry localTran, tInfo')
      where
        utcM' = addUtcTransition tran tInfo' utcM
        calDateM' = addCalDateTransition prevEntry before prevTI calDateM
        localTran = applyOffset (tiOffset tInfo) $ tran
        before = Entry . flip minus oneSecond . applyOffset (utcOffset prevTI) $ tran
        tInfo = tInfos !! idx
        tInfo' = mkTI tInfo

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
