module Data.HodaTime.TimeZone
(
   utc
  ,localZone
  ,timeZone
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Platform
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)

utc :: (MonadThrow n, MonadIO m) => m (n TimeZone)
utc = do
  res <- loadUTC
  return . fmap (\(utcM, calDateM, leaps) -> TimeZone UTC utcM calDateM leaps) $ res

timeZone :: (MonadThrow n, MonadIO m) => String -> m (n TimeZone)
timeZone tzName = do
  res <- loadTimeZone tzName
  return . fmap (\(utcM, calDateM, leaps) -> TimeZone (Zone tzName) utcM calDateM leaps) $ res

localZone :: (MonadIO m, MonadThrow n) => m (n TimeZone)
localZone = do
  res <- loadLocalZone
  return . fmap (\(utcM, calDateM, leaps, tzName) -> TimeZone (Zone tzName) utcM calDateM leaps) $ res