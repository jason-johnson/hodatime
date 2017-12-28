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
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.TimeZone.Platform

-- | Load the UTC time zone
utc :: IO TimeZone
utc = do
  (utcM, calDateM, leaps, tExprDetails) <- loadUTC
  return $ TimeZone UTC utcM calDateM leaps tExprDetails

-- | Load the specified time zone.  The time zone name should be in the standard format (e.g. "Europe/Paris")
timeZone :: String -> IO TimeZone
timeZone tzName = do
  (utcM, calDateM, leaps, tExprDetails) <- loadTimeZone tzName
  return $ TimeZone (Zone tzName) utcM calDateM leaps tExprDetails

-- | Load the locally configured time zone (operating system configuration dependant)
localZone :: IO TimeZone
localZone = do
  (utcM, calDateM, leaps, tExprDetails, tzName) <- loadLocalZone
  return $ TimeZone (Zone tzName) utcM calDateM leaps tExprDetails