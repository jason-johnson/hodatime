module Data.HodaTime.TimeZone.Platform
(
   loadLocalZone
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

data TimeZoneDoesNotExistException = TimeZoneDoesNotExistException
  deriving (Typeable, Show)

instance Exception TimeZoneDoesNotExistException

data TZoneDBCorruptException = TZoneDBCorruptException
  deriving (Typeable, Show)

instance Exception TZoneDBCorruptException

tzdbDir :: FilePath
tzdbDir = "/usr" </> "share" </> "zoneinfo"

loadTimeZone :: (MonadThrow n, MonadIO m) => String -> m (n TimeZone)
loadTimeZone tzName = do
  let file = tzdbDir </> tzName
  loadZoneFromOlsonFile (Zone tzName) file

loadLocalZone :: (MonadIO m, MonadThrow n) => m (n TimeZone)
loadLocalZone = do
  let file = "/etc" </> "localtime"
  tzPath <- liftIO . readSymbolicLink $ file
  let tzName = Zone . drop 1 . fromMaybe " unknown" . stripPrefix tzdbDir $ tzPath  -- TODO: We should detect if this failed and throw an error, not set to unknown
  loadZoneFromOlsonFile tzName file

-- helper functions

loadZoneFromOlsonFile :: (MonadIO m, MonadThrow n) => TZIdentifier -> FilePath -> m (n TimeZone)
loadZoneFromOlsonFile tzName file = do
  exists <- liftIO . doesFileExist $ file
  unless exists $ liftIO . throwM $ TimeZoneDoesNotExistException
  mTrans <- fmap getTransitions (liftIO . BS.readFile $ file)
  return . fmap (\(utcM, calDateM, _) -> TimeZone tzName utcM calDateM) $ mTrans