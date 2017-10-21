module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,fixedOffsetZone
  ,loadLocalZone
  ,loadTimeZone
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Olson
import Data.HodaTime.Instant.Internal (bigBang)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files (readSymbolicLink)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
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

-- interface

loadUTC :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadUTC = loadTimeZone "UTC"

fixedOffsetZone :: String -> Int -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
fixedOffsetZone tzName offset = do
  leapM' <- loadLeaps leapM
  return (utcM, calDateM, leapM')
    where
      utcM = addUtcTransition bigBang tInfo emptyUtcTransitions
      calDateM = addCalDateTransition Smallest Largest tInfo emptyCalDateTransitions
      leapM = addLeapTransition bigBang 0 emptyLeapsMap
      tInfo = TransitionInfo offset False tzName

loadTimeZone :: String -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadTimeZone tzName = do
  loadZoneFromOlsonFile $ tzdbDir </> tzName

loadLocalZone :: IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String)
loadLocalZone = do
  let file = "/etc" </> "localtime"
  tzPath <- readSymbolicLink $ file
  let tzName = drop 1 . fromMaybe " unknown" . stripPrefix tzdbDir $ tzPath  -- TODO: We should detect if this failed and throw an error, not set to unknown
  (utcM, calDateM, leaps)  <- loadZoneFromOlsonFile file
  return (utcM, calDateM, leaps, tzName)

-- helper functions

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

loadZoneFromOlsonFile :: FilePath -> IO (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap)
loadZoneFromOlsonFile file = do
  exists <- doesFileExist $ file
  unless exists (throwIO TimeZoneDoesNotExistException)
  bs <- BS.readFile $ file
  (utcM, calDateM, leapM) <- getTransitions bs
  leapM' <- loadLeaps leapM
  return (utcM, calDateM, leapM')

-- On Mac platform, leaps aren't stored anywhere so we have to load them seperately

loadLeaps :: LeapsMap -> IO LeapsMap
loadLeaps = return . mergeLeapMaps leaps
  where
    leaps = importLeaps []