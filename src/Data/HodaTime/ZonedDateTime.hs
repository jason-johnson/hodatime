-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HodaTime.Instant
-- Copyright   :  (C) 2017 Jason Johnson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jason Johnson <jason.johnson.081@gmail.com>
-- Stability   :  experimental
-- Portability :  POSIX, Windows
--
-- This is the module for 'ZonedDateTime'.  A 'ZonedDateTime' is a universal time, it represents the same moment anywhere in the world and is completely
-- unambiguous.  Each instance of a 'ZonedDateTime' corresponds to exactly one point on a continuous time line.
----------------------------------------------------------------------------
module Data.HodaTime.ZonedDateTime
(
  -- * Types
   ZonedDateTime
  -- * Constructors
  ,fromCalendarDateTimeLeniently
  ,fromCalendarDateTimeStrictly
  -- * Math
  -- * Conversion
  ,toLocalDateTime
  ,toLocalDate
  ,toLocalTime
  -- * Accessors
  ,inDst
  -- * Special constructors
  ,fromCalendarDateTimeAll
  ,resolve
  -- * Exceptions
  ,DateTimeDoesNotExistException(..)
  ,DateTimeAmbiguousException(..)
)
where

import Data.HodaTime.ZonedDateTime.Internal
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime(..), CalendarDate(..), IsCalendarDateTime(..), IsCalendar(..), LocalTime)
import Data.HodaTime.LocalTime.Internal (second)
import Data.HodaTime.TimeZone.Internal (TimeZone(..), TransitionInfo(..), activeLeapsFor, calDateTransitionsFor, aroundCalDateTransition)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)

-- exceptions

data DateTimeDoesNotExistException = DateTimeDoesNotExistException
  deriving (Typeable, Show)

instance Exception DateTimeDoesNotExistException

data DateTimeAmbiguousException = DateTimeAmbiguousException
  deriving (Typeable, Show)

instance Exception DateTimeAmbiguousException

-- constructors

-- | Returns the mapping of this 'CalendarDateTime' within the given 'TimeZone', with "lenient" rules applied such that ambiguous values map to the earlier of the alternatives,
--   and "skipped" values are shifted forward by the duration of the "gap".
fromCalendarDateTimeLeniently :: (IsCalendar cal, IsCalendarDateTime cal) => CalendarDateTime cal -> TimeZone -> ZonedDateTime cal
fromCalendarDateTimeLeniently = resolve ambiguous skipped
  where
    ambiguous zdt _ = zdt
    skipped (ZonedDateTime _ _ (TransitionInfo bOff _ _)) (ZonedDateTime cdt tz ti@(TransitionInfo aOff _ _)) = ZonedDateTime cdt' tz ti
      where
        cdt' = modify (\s -> s + aOff - bOff) second cdt
        modify f l = head . l ((:[]) . f)

-- | Returns the mapping of this 'CalendarDateTime' within the given 'TimeZone', with "strict" rules applied such that ambiguous or skipped date times
--   return the requested failure response (e.g. Nothing, Left, exception, etc.)
fromCalendarDateTimeStrictly :: (MonadThrow m, IsCalendarDateTime cal) => CalendarDateTime cal -> TimeZone -> m (ZonedDateTime cal)
fromCalendarDateTimeStrictly cdt = go . fromCalendarDateTimeAll cdt
  where
    go [] = throwM $ DateTimeDoesNotExistException
    go [zdt] = return zdt
    go _ = throwM $ DateTimeAmbiguousException

-- | Return all 'ZonedDateTime' entries for a specific 'CalendarDateTime' in a 'TimeZone'. Normally this would be one, but in the case that a time
-- occurs twice in a zone (i.e. due to daylight savings time change) both would be returned.  Also, if the time does not occur at all, an empty list
-- will be returned.  This method allows the user to choose exactly what to do in the case of ambigiuty.
fromCalendarDateTimeAll :: IsCalendarDateTime cal => CalendarDateTime cal -> TimeZone -> [ZonedDateTime cal]
fromCalendarDateTimeAll cdt tz@(TimeZone _ _ calDateM _) = zdts
  where
    instant = toUnadjustedInstant cdt
    zdts = fmap mkZdt . calDateTransitionsFor instant $ calDateM
    mkZdt = ZonedDateTime cdt tz

-- | Takes two functions to determine how to resolve a 'CalendarDateTime' to a 'ZonedDateTime' in the case of ambiguity or skipped times.  The first
-- function is for the ambigous case and is past the first matching 'ZonedDateTime', followed by the second match. The second function is for the case
-- that the 'CalendarDateTime' doesn't exist in the 'TimeZone' (e.g. in a spring-forward situation, there will be a missing hour), the first
-- 'ZonedDateTime' will be the the last time before the gap and the second will be the first time after the gap.
resolve ::
  IsCalendarDateTime cal =>
  (ZonedDateTime cal -> ZonedDateTime cal -> ZonedDateTime cal) ->
  (ZonedDateTime cal -> ZonedDateTime cal -> ZonedDateTime cal) ->
  CalendarDateTime cal ->
  TimeZone ->
  ZonedDateTime cal
resolve ambiguous skipped cdt tz@(TimeZone _ _ calDateM _) = go . fmap mkZdt . calDateTransitionsFor instant $ calDateM
  where
    instant = toUnadjustedInstant cdt
    (before, after) = aroundCalDateTransition instant $ calDateM
    mkZdt = ZonedDateTime cdt tz
    go [] = skipped (mkZdt before) (mkZdt after)
    go [zdt] = zdt
    go (zdt1:zdt2:[]) = ambiguous zdt1 zdt2
    go _ = error "misconfiguration: more than 2 dates returns from calDateTransitionsFor"

-- conversion

-- | Return the 'CalendarDateTime' represented by this 'ZonedDateTime'.
toLocalDateTime :: ZonedDateTime cal -> CalendarDateTime cal
toLocalDateTime (ZonedDateTime cdt _  _) = cdt

-- | Return the 'CalendarDate' represented by this 'ZonedDateTime'.
toLocalDate :: ZonedDateTime cal -> CalendarDate cal
toLocalDate (ZonedDateTime (CalendarDateTime cd _) _  _) = cd

-- | Return the 'LocalTime' represented by this 'ZonedDateTime'.
toLocalTime :: ZonedDateTime cal -> LocalTime
toLocalTime (ZonedDateTime (CalendarDateTime _ lt) _  _) = lt

-- Accessors

-- | Return a 'Bool' specifying if this 'ZonedDateTime' is currently in Daylight savings time.
inDst :: ZonedDateTime cal -> Bool
inDst (ZonedDateTime _ _ (TransitionInfo _ isInDst _)) = isInDst