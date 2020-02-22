-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Instant
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This module deals with loading TimeZone information with which to construct ZonedDateTimes.
----------------------------------------------------------------------------
module Data.HodaTime.TimeZone
(
  -- * Types
   TimeZone
  -- * Constructors
  ,utc
  ,localZone
  ,timeZone
  ,availableZones
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Platform

-- | Load the UTC time zone
utc :: IO TimeZone
utc = do
  (utcM, calDateM) <- loadUTC
  return $ TimeZone UTC utcM calDateM

-- | Load the specified time zone.  The time zone name should be in the format returned by `availableZones`
timeZone :: String -> IO TimeZone
timeZone tzName = do
  (utcM, calDateM) <- loadTimeZone tzName
  return $ TimeZone (Zone tzName) utcM calDateM

-- | Load the locally configured time zone (operating system configuration dependant)
localZone :: IO TimeZone
localZone = do
  (utcM, calDateM, tzName) <- loadLocalZone
  return $ TimeZone (Zone tzName) utcM calDateM

-- | List all time zones available to hodatime
availableZones :: IO [String]
availableZones = loadAvailableZones