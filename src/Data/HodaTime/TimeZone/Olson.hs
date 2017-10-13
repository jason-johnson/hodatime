module Data.HodaTime.TimeZone.Olson
(
  getTransitions
)
where

import Data.HodaTime.TimeZone.Internal

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get (Get, getWord8, getWord32be, getByteString, runGetOrFail, skip, isEmpty)
import Data.Word (Word8)
import Control.Monad (unless, replicateM_, replicateM)
import Data.Int (Int32)
import Control.Applicative ((<$>), (<*>), ZipList(..))
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.Instant.Internal (Instant)

data TransInfo = TransInfo { tiOffset :: Int, tiIsDst :: Bool, abbr :: String }
  deriving (Eq, Show)

getTransitions :: L.ByteString -> Either String (UtcTransitionsMap, [(Instant, Int)], LeapsMap)
getTransitions bs = case runGetOrFail getTransitions' bs of
  Left (_, _, msg) -> Left msg
  Right (_, _, xs) -> Right xs
  where
    getTransitions' = do
      (magic, _version, ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen) <- getHeader
      unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)
      (utcM, wall, leapsMap) <- getPayload transcnt ttypecnt abbrlen leapcnt ttisstdcnt ttisgmtcnt
      finished <- isEmpty
      unless finished $ fail "unprocessed data still in olson file"
      return (utcM, wall, leapsMap)

-- Get combinators

getHeader :: Get (String, Word8, Int, Int, Int, Int, Int, Int)
getHeader = do
  magic <- (toString . B.unpack) <$> getByteString 4
  version <- getWord8
  replicateM_ 15 getWord8 -- skip reserved section
  [ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen] <- replicateM 6 get32bitInt
  unless
    (ttisgmtcnt == ttisstdcnt && ttisstdcnt == ttypecnt)
    (fail $ "format issue, sizes don't match: ttisgmtcnt = " ++ show ttisgmtcnt ++ ", ttisstdcnt = " ++ show ttisstdcnt ++ ", ttypecnt = " ++ show ttypecnt)
  return (magic, version, ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen)

getLeapInfo :: Get (Instant, Int)
getLeapInfo = do
  instant <- fromSecondsSinceUnixEpoch <$> get32bitInt
  lOffset <- get32bitInt
  return (instant, lOffset)

getPayload :: Int -> Int -> Int -> Int -> Int -> Int -> Get (UtcTransitionsMap, [(Instant, Int)], LeapsMap)
getPayload transCount typeCount abbrLen leapCount isStdCount isGmtCount = do
  transitions <- replicateM transCount $ fromSecondsSinceUnixEpoch <$> get32bitInt
  indexes <- replicateM transCount get8bitInt
  types <- replicateM typeCount $ (,,) <$> get32bitInt <*> getBool <*> get8bitInt
  abbrs <- (toString . B.unpack) <$> getByteString abbrLen
  leaps <- replicateM leapCount getLeapInfo
  skip $ isStdCount + isGmtCount
  let tInfos = mapTransitionInfos abbrs types
  let (utcM, wall) = partitionAndZip (zip transitions indexes) tInfos
  return (utcM, wall, importLeaps leaps)

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

get8bitInt :: Get Int
get8bitInt = fmap fromIntegral getWord8

getInt32 :: Get Int32
getInt32 = fmap fromIntegral getWord32be

get32bitInt :: Get Int
get32bitInt = fmap fromIntegral getInt32

-- helper fucntions

mapTransitionInfos :: String -> [(Int, Bool, Int)] -> [TransInfo]
mapTransitionInfos abbrs = fmap toTI
  where
    toTI (gmt, isdst, offset) = TransInfo gmt isdst (getAbbr offset abbrs)
    getAbbr offset = takeWhile (/= '\NUL') . drop offset

partitionAndZip :: [(Instant, Int)] -> [TransInfo] -> (UtcTransitionsMap, [(Instant, Int)])
partitionAndZip transAndIndexes tInfos = foldr select (emptyTransitions,[]) transAndIndexes
  where
    select (tran, idx) (utcM , wallclock) = (addTransition tran tInfo' utcM, wallclock)
      where
        tInfo = tInfos !! idx
        tInfo' = TransitionInfo (tiOffset tInfo) (tiIsDst tInfo) (abbr tInfo)

toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)
