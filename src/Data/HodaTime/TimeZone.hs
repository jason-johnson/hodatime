module Data.HodaTime.TimeZone
(
   utc
  ,localZone
  ,timeZone
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Platform

utc :: IO TimeZone
utc = do
  (utcM, calDateM, leaps) <- loadUTC
  return $ TimeZone UTC utcM calDateM leaps

timeZone :: String -> IO TimeZone
timeZone tzName = do
  (utcM, calDateM, leaps) <- loadTimeZone tzName
  return $ TimeZone (Zone tzName) utcM calDateM leaps

localZone :: IO TimeZone
localZone = do
  (utcM, calDateM, leaps, tzName) <- loadLocalZone
  return $ TimeZone (Zone tzName) utcM calDateM leaps