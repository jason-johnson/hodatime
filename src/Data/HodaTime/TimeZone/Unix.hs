module Data.HodaTime.TimeZone.Unix
(
   loadUTC
  ,fixedOffsetZone
  ,loadLocalZone
  ,loadTimeZone
  ,defaultLoadZoneFromOlsonFile
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Olson
import Data.HodaTime.Instant.Internal (bigBang)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files (readSymbolicLink)
import Data.List (intercalate)
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Typeable (Typeable)

-- exceptions

data TimeZoneDoesNotExistException = TimeZoneDoesNotExistException
  deriving (Typeable, Show)

instance Exception TimeZoneDoesNotExistException

data TZoneDBCorruptException = TZoneDBCorruptException
  deriving (Typeable, Show)

instance Exception TZoneDBCorruptException

type LoadZoneFromOlsonFile = FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
type LoadLeaps = LeapsMap -> IO LeapsMap

-- interface

loadUTC :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadUTC loadZoneFromOlsonFile = loadTimeZone loadZoneFromOlsonFile "UTC"

fixedOffsetZone :: LoadLeaps -> String -> Int -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, TransitionInfo)
fixedOffsetZone loadLeaps tzName offset = do
  leapM' <- loadLeaps leapM
  return (utcM, calDateM, leapM', tInfo)
    where
      utcM = addUtcTransition bigBang tInfo emptyUtcTransitions
      calDateM = addCalDateTransition Smallest Largest tInfo emptyCalDateTransitions
      leapM = addLeapTransition bigBang 0 emptyLeapsMap
      tInfo = TransitionInfo offset False tzName

loadTimeZone :: LoadZoneFromOlsonFile -> String -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadTimeZone loadZoneFromOlsonFile tzName = do
  loadZoneFromOlsonFile $ tzdbDir </> tzName

loadLocalZone :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String)
loadLocalZone loadZoneFromOlsonFile = do
  let file = "/etc" </> "localtime"
  tzPath <- readSymbolicLink $ file
  let tzName = timeZoneFromPath $ tzPath
  (utcM, calDateM, leaps)  <- loadZoneFromOlsonFile file
  return (utcM, calDateM, leaps, tzName)

-- helper functions

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

defaultLoadZoneFromOlsonFile :: LoadZoneFromOlsonFile
defaultLoadZoneFromOlsonFile file = do
  exists <- doesFileExist $ file
  unless exists (throwIO TimeZoneDoesNotExistException)
  bs <- BS.readFile $ file
  (utcM, calDateM, leapM) <- getTransitions bs
  return (utcM, calDateM, leapM)

-- helper functions

timeZoneFromPath :: FilePath -> String
timeZoneFromPath = intercalate "/" . drp . foldr collect [[]]
  where
    drp = drop 1 . dropWhile (/= "zoneinfo")
    collect ch l@(x:xs)
      | ch == '/' = []:l
      | otherwise = (ch:x):xs
    collect _ _ = error "impossible: only used to prove pattern is exhaustive"