module Data.HodaTime.TimeZone.Unix
(
   loadUTC
  ,loadLocalZone
  ,loadTimeZone
  ,loadAvailableZones
  ,defaultLoadZoneFromOlsonFile
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Olson
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files (readSymbolicLink, getFileStatus, isDirectory)
import System.FilePath.Posix (makeRelative)
import Data.List (intercalate)
import Control.Exception (Exception, throwIO)
import Control.Monad (unless, forM)
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
  let tzName = timeZoneNameFromPath $ tzPath
  (utcM, calDateM)  <- loadZoneFromOlsonFile file
  return (utcM, calDateM, tzName)

loadAvailableZones :: IO [String]
loadAvailableZones = traverseDir tzdbDir
  where
    toZoneName file = makeRelative tzdbDir file
    toResult file = do
      bs <- BS.readFile $ file
      let valid = isOlsonFile bs
      if valid
        then return [toZoneName file]
        else return []
    traverseDir top = do
      ds <- getDirectoryContents top
      paths <- forM (filter (not . flip elem [".", ".."]) ds) $ \d -> do
        let path = top </> d
        s <- getFileStatus path
        if isDirectory s
          then traverseDir path
          else toResult path
      return (concat paths)

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

timeZoneNameFromPath :: FilePath -> String
timeZoneNameFromPath = intercalate "/" . drp . foldr collect [[]]
  where
    drp = drop 1 . dropWhile (/= "zoneinfo")
    collect ch l@(x:xs)
      | ch == '/' = []:l
      | otherwise = (ch:x):xs
    collect _ _ = error "impossible: only used to prove pattern is exhaustive"