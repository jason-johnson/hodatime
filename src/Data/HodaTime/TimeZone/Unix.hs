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

type LoadZoneFromOlsonFile = FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)

-- interface

loadUTC :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)
loadUTC loadZoneFromOlsonFile = loadTimeZone loadZoneFromOlsonFile "UTC"

fixedOffsetZone :: String -> Int -> (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails, TransitionInfo)
fixedOffsetZone tzName offset = (utcM, calDateM, Nothing, tInfo)
    where
      utcM = addUtcTransition bigBang tInfo emptyUtcTransitions
      calDateM = addCalDateTransition Smallest Largest tInfo emptyCalDateTransitions
      tInfo = TransitionInfo offset False tzName

loadTimeZone :: LoadZoneFromOlsonFile -> String -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails)
loadTimeZone loadZoneFromOlsonFile tzName = do
  loadZoneFromOlsonFile $ tzdbDir </> tzName

loadLocalZone :: LoadZoneFromOlsonFile -> IO (UtcTransitionsMap, CalDateTransitionsMap, Maybe TransitionExpressionDetails, String)
loadLocalZone loadZoneFromOlsonFile = do
  let file = "/etc" </> "localtime"
  tzPath <- readSymbolicLink $ file
  let tzName = timeZoneFromPath $ tzPath
  (utcM, calDateM, tExprDetails)  <- loadZoneFromOlsonFile file
  return (utcM, calDateM, tExprDetails, tzName)

-- helper functions

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

defaultLoadZoneFromOlsonFile :: LoadZoneFromOlsonFile
defaultLoadZoneFromOlsonFile file = do
  exists <- doesFileExist $ file
  unless exists (throwIO TimeZoneDoesNotExistException)
  bs <- BS.readFile $ file
  (utcM, calDateM, tExprDetails) <- getTransitions bs
  return (utcM, calDateM, tExprDetails)

-- helper functions

timeZoneFromPath :: FilePath -> String
timeZoneFromPath = intercalate "/" . drp . foldr collect [[]]
  where
    drp = drop 1 . dropWhile (/= "zoneinfo")
    collect ch l@(x:xs)
      | ch == '/' = []:l
      | otherwise = (ch:x):xs
    collect _ _ = error "impossible: only used to prove pattern is exhaustive"