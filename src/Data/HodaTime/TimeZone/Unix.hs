module Data.HodaTime.TimeZone.Unix
(
   loadUTC
  ,loadLocalZone
  ,loadTimeZone
  ,defaultLoadZoneFromOlsonFile
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Olson
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

type LoadZoneFromOlsonFile = FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap)

-- interface

loadUTC :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadUTC loadZoneFromOlsonFile = loadTimeZone loadZoneFromOlsonFile "UTC"

loadTimeZone :: LoadZoneFromOlsonFile -> String -> IO (UtcTransitionsMap, CalDateTransitionsMap)
loadTimeZone loadZoneFromOlsonFile tzName = do
  loadZoneFromOlsonFile $ tzdbDir </> tzName

loadLocalZone :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap, String)
loadLocalZone loadZoneFromOlsonFile = do
  let file = "/etc" </> "localtime"
  tzPath <- readSymbolicLink $ file
  let tzName = timeZoneFromPath $ tzPath
  (utcM, calDateM)  <- loadZoneFromOlsonFile file
  return (utcM, calDateM, tzName)

-- helper functions

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

defaultLoadZoneFromOlsonFile :: LoadZoneFromOlsonFile
defaultLoadZoneFromOlsonFile file = do
  exists <- doesFileExist $ file
  unless exists (throwIO TimeZoneDoesNotExistException)
  bs <- BS.readFile $ file
  (utcM, calDateM) <- getTransitions bs
  return (utcM, calDateM)

-- helper functions

timeZoneFromPath :: FilePath -> String
timeZoneFromPath = intercalate "/" . drp . foldr collect [[]]
  where
    drp = drop 1 . dropWhile (/= "zoneinfo")
    collect ch l@(x:xs)
      | ch == '/' = []:l
      | otherwise = (ch:x):xs
    collect _ _ = error "impossible: only used to prove pattern is exhaustive"