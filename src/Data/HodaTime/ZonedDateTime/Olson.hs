module Data.HodaTime.ZonedDateTime.Olson
(
  getTransitions
)
where

import Data.HodaTime.ZonedDateTime.Internal

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get (Get, getWord8, getWord32be, getByteString, runGetOrFail)
import Data.Word (Word8)
import Control.Monad (unless, replicateM_, replicateM, liftM, filterM)
import Data.Int (Int32)
import Control.Applicative ((<$>), (<*>), ZipList(..))
import Data.List (nub)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

testIt = testIt' "/etc/localtime"

testIt' :: FilePath -> IO [Transition]
testIt' path = fmap (either (const []) id . getTransitions) $ L.readFile path

--testAll = fmap (map snd) $ mapDir testIt' "/usr/share/zoneinfo"

mapDir :: (FilePath -> IO (String, t)) -> FilePath -> IO [(String, t)]
mapDir proc fp = go'
    where
        go' = go [] >>= return . filter (\(token, _) -> "unknown magic" /= take 13 token)
        go xs = do
            isFile <- doesFileExist fp
            if isFile
                then proc fp >>= return . (: xs)    -- process the file
                else getDirectoryContents fp >>= liftM concat . mapM (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])

getTransitions :: L.ByteString -> Either String Transitions
getTransitions bs = case runGetOrFail getTransitions' bs of
    Left (_, _, msg) -> Left msg
    Right (_, _, xs) -> Right xs
    where
        getTransitions' = do
            magic <- (toASCII . B.unpack) <$> getByteString 4
            unless (magic == "TZif") (fail $ "unknown magic: " ++ magic)            -- We could consider creating an error type for this
            _version <- getWord8
            replicateM_ 15 getWord8 -- skip reserved section
            [ttisgmtcnt, ttisstdcnt, leapcnt, transcnt, ttypecnt, abbrlen] <- replicateM 6 get32bitInt
            transitions <- replicateM transcnt $ fromSecondsSinceUnixEpoch <$> get32bitInt
            indexes <- replicateM transcnt get8bitInt
            ttypes <- replicateM ttypecnt $ (,,) <$> get32bitInt <*> getBool <*> get8bitInt
            abbrs <- (toASCII . B.unpack) <$> getByteString abbrlen
            _leaps <- replicateM leapcnt getLeapInfo
            ttisstds <- replicateM ttisstdcnt getBool
            ttisgmts <- replicateM ttisgmtcnt getBool
            return $ zipTransitions (zipTransitionTypes abbrs ttypes ttisstds ttisgmts) transitions indexes

zipTransitionTypes :: String -> [(Int, Bool, Int)] -> [Bool] -> [Bool] -> [TransitionInfo]
zipTransitionTypes abbrs = zip3With toTI
    where
        toTI (gmt, isdst, offset) = TransitionInfo gmt isdst (getAbbr offset abbrs)
        getAbbr offset = takeWhile (/= '\NUL') . drop offset

zipTransitions :: [TransitionInfo] -> [Instant] -> [Int] -> Transitions
zipTransitions tis trans = foldr (\(i, idx) im -> addTransitionInfo i (tis !! idx) im) mkTransitions . zip trans

zip3With :: (a3 -> a2 -> a1 -> a) -> [a3] -> [a2] -> [a1] -> [a]                  -- TODO: Can we use base zipWith3 here?
zip3With f xs ys zs = getZipList $ f <$> ZipList xs <*> ZipList ys <*> ZipList zs

getLeapInfo :: Get (Integer, Int)
getLeapInfo = do
    lTime <- fmap toInteger get32bitInteger
    lOffset <- get32bitInt
    return (lTime, lOffset)

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

get8bitInt :: Get Int
get8bitInt = fmap fromIntegral getWord8

getInt32 :: Get Int32
getInt32 = fmap fromIntegral getWord32be

get32bitInt :: Get Int
get32bitInt = fmap fromIntegral getInt32

get32bitInteger :: Get Integer
get32bitInteger = fmap fromIntegral getInt32

toASCII :: [Word8] -> String
toASCII = map (toEnum . fromIntegral)
