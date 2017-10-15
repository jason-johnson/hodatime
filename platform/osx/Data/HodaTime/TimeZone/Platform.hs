module Data.HodaTime.TimeZone.Platform
(
   loadUTC
  ,loadLocalZone
  ,loadTimeZone
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Olson
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files (readSymbolicLink)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable)

-- exceptions

data TimeZoneDoesNotExistException = TimeZoneDoesNotExistException
  deriving (Typeable, Show)

instance Exception TimeZoneDoesNotExistException

data TZoneDBCorruptException = TZoneDBCorruptException
  deriving (Typeable, Show)

instance Exception TZoneDBCorruptException

-- interface

loadUTC :: (MonadIO m, MonadThrow n) => m (n (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap))
loadUTC = loadTimeZone "UTC"

loadTimeZone :: (MonadThrow n, MonadIO m) => String -> m (n (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap))
loadTimeZone tzName = do
  loadZoneFromOlsonFile $ tzdbDir </> tzName

loadLocalZone :: (MonadIO m, MonadThrow n) => m (n (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap, String))
loadLocalZone = do
  let file = "/etc" </> "localtime"
  tzPath <- liftIO . readSymbolicLink $ file
  let tzName = drop 1 . fromMaybe " unknown" . stripPrefix tzdbDir $ tzPath  -- TODO: We should detect if this failed and throw an error, not set to unknown
  res <- loadZoneFromOlsonFile file
  return . fmap (\(utcM, calDateM, leaps) -> (utcM, calDateM, leaps, tzName)) $ res

-- helper functions

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

loadZoneFromOlsonFile :: (MonadIO m, MonadThrow n) => FilePath -> m (n (UtcTransitionsMap, CalDateTransitionsMap, LeapsMap))
loadZoneFromOlsonFile file = do
  exists <- liftIO . doesFileExist $ file
  if exists then
    do
      bs <- liftIO . BS.readFile $ file
      return $ do
        (utcM, calDateM, _) <- getTransitions bs
        return (utcM, calDateM, loadLeaps')
  else return $ throwM TimeZoneDoesNotExistException

-- On Mac platform, leaps aren't stored anywhere so we have to load them seperately
loadLeaps' :: LeapsMap
loadLeaps' = importLeaps []

loadLeaps :: (MonadIO m, MonadThrow n) => m (n LeapsMap)
loadLeaps = return . return $ loadLeaps'