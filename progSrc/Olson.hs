module Olson
(
   ParseException(..)
  ,Header(..)
  ,Transition(..)
  ,runGet
  ,getHeader
  ,getSkipped
  ,getPayload
  ,get32bitInteger
  ,get64bitInteger
  ,isEmpty
  ,getPosixTZ
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get (Get, getWord8, getWord32be, getWord64be, getByteString, runGetOrFail, skip, isEmpty, getRemainingLazyByteString)
import Data.Word (Word8)
import Control.Monad (unless, replicateM)
import Data.List (foldl')
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)

data ParseException = ParseException String Int
  deriving (Typeable, Show)

instance Exception ParseException

data Header = Header
  {
     olsonVersion :: Word8
    ,isGmtCount :: Int
    ,isStdCount :: Int
    ,leapsCount :: Int
    ,transitionsCount :: Int
    ,typeCount :: Int
    ,abbrLength :: Int
  }
  deriving Show

data TransInfo = TransInfo { tiOffset :: Int, _tiIsDst :: Bool, _tiAbbr :: String }
  deriving (Eq, Show)

data Transition a = Transition { transition :: a, transOffset :: Int, transIsDst :: Bool, transAbbr :: String }
  deriving (Eq, Show)

reservedSectionSize :: Int
reservedSectionSize = 15

runGet :: MonadThrow m => Get a -> L.ByteString -> m (L.ByteString, a)
runGet get bs = case runGetOrFail get bs of
  Left (_, consumed, msg) -> throwM $ ParseException msg (fromIntegral consumed)
  Right (bs', _, xs) -> return (bs', xs)

-- Get combinators

getHeader :: Get Header
getHeader = do
  magic <- (toString . B.unpack) <$> getByteString 4
  unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)
  version <- getWord8
  skip reservedSectionSize
  [ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen] <- replicateM 6 get32bitInt
  unless
    (ttisgmtcnt == ttisstdcnt && ttisstdcnt == ttypecnt)
    (fail $ "format issue, sizes don't match: ttisgmtcnt = " ++ show ttisgmtcnt ++ ", ttisstdcnt = " ++ show ttisstdcnt ++ ", ttypecnt = " ++ show ttypecnt)
  return $ Header version ttisgmtcnt ttisstdcnt leapcnt transcnt ttypecnt abbrlen

getSkipped :: Int -> Get ()
getSkipped i = do
  skip i


getLeapInfo :: Num a => Get a -> Get (a, Int)
getLeapInfo getInteger = do
  instant <- getInteger
  lOffset <- get32bitInt
  return (instant, lOffset)

getPayload :: Num a => Header -> Get a -> Get [Transition a]
getPayload (Header _ ttisgmtcnt ttisstdcnt leapcnt transcnt ttypecnt abbrlen) getInteger = do
  transitions <- replicateM transcnt getInteger
  indexes <- replicateM transcnt get8bitInt
  types <- replicateM ttypecnt $ (,,) <$> get32bitInt <*> getBool <*> get8bitInt
  abbrs <- (toString . B.unpack) <$> getByteString abbrlen
  leaps <- replicateM leapcnt $ getLeapInfo getInteger       -- TODO: when we have good tests in place, see if we can turn this into a fold and create the leaps map right here
  skip $ ttisstdcnt + ttisgmtcnt
  let tInfos = mapTransitionInfos abbrs types
  return . buildTransitionMaps (zip transitions indexes) $ tInfos

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

get8bitInt :: Get Int
get8bitInt = fmap fromIntegral getWord8

get32bitInt :: Get Int
get32bitInt = fmap fromIntegral getWord32be

get32bitInteger :: Get Integer
get32bitInteger = fmap fromIntegral getWord32be

get64bitInteger :: Get Integer
get64bitInteger = fmap fromIntegral getWord64be

-- helper fucntions

mapTransitionInfos :: String -> [(Int, Bool, Int)] -> [TransInfo]
mapTransitionInfos abbrs = fmap toTI
  where
    toTI (gmt, isdst, offset) = TransInfo gmt isdst (getAbbr offset abbrs)
    getAbbr offset = takeWhile (/= '\NUL') . drop offset

buildTransitionMaps :: Num a => [(a, Int)] -> [TransInfo] -> [Transition a]
buildTransitionMaps transAndIndexes tInfos = foldr go [] transAndIndexes
  where
    go (tran, idx) trans = (tInfo'' : trans)
      where
        mkTI (TransInfo offset isDst abbr) trs = Transition trs offset isDst abbr
        localTran = applyOffset (tiOffset tInfo) $ tran
        tInfo = if idx > length tInfos then (error $ "got OoB index: " ++ show idx) else tInfos !! idx
        tInfo' = mkTI tInfo localTran
        tInfo'' = mkTI tInfo tran

applyOffset :: Num a => Int -> a -> a
applyOffset off i = i + fromIntegral off

toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)

getPosixTZ :: Get String
getPosixTZ = do
  nl <- toEnum . fromIntegral <$> getWord8
  unless (nl == '\n') (fail $ "POSIX TZ string preceded by non-newline:" ++ show nl)
  posixTZ <- fmap (L.takeWhile (/= 10)) getRemainingLazyByteString
  return . toString . L.unpack $ posixTZ
