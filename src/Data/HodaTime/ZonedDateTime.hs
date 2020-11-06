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
  ,fromInstant
  -- * Math
  -- * Conversion
  ,toCalendarDateTime
  ,toCalendarDate
  ,toLocalTime
  ,toInstant
  -- * Accessors
  ,inDst
  ,zoneAbbreviation
  -- * Special constructors
  ,fromCalendarDateTimeAll
  ,resolve
  -- * Exceptions
  ,DateTimeDoesNotExistException(..)
  ,DateTimeAmbiguousException(..)
)
where

import Data.HodaTime.Instant
import Data.HodaTime.ZonedDateTime.Internal
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime(..), CalendarDate(..), IsCalendarDateTime(..), IsCalendar(..), LocalTime)
import Data.HodaTime.LocalTime.Internal (second)
import Data.HodaTime.Duration (fromSeconds)
import Data.HodaTime.Offset.Internal (Offset(..))
import Data.HodaTime.TimeZone.Internal (TimeZone, TransitionInfo(..), calDateTransitionsFor, aroundCalDateTransition)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (Typeable)

-- exceptions

-- TODO: find a way to get the offending CalendarDateTime into the exception so that if this is thrown in deeply nested code users can figure out
-- TODO: which date caused it.  The current problem is that "instance Exception" doesn't work if there is a type variable, even if the data type
-- TODO: itself is typeable
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
    skipped (ZonedDateTime _ _ (TransitionInfo (Offset bOff) _ _)) (ZonedDateTime cdt tz ti@(TransitionInfo (Offset aOff) _ _)) = ZonedDateTime cdt' tz ti
      where
        cdt' = modify (\s -> s + aOff - bOff) second cdt
        modify f l = head . l ((:[]) . f)                 -- TODO: We may want to break down and define the 3 lens primitives we need somewhere

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
fromCalendarDateTimeAll cdt tz = zdts
  where
    instant = toUnadjustedInstant cdt
    zdts = fmap mkZdt . calDateTransitionsFor instant $ tz
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
  ZonedDateTime cal -- TODO: This function should probably allow failure
resolve ambiguous skipped cdt tz = go . fmap mkZdt . calDateTransitionsFor instant $ tz
  where
    instant = toUnadjustedInstant cdt
    (before, after) = aroundCalDateTransition instant $ tz
    mkZdt = ZonedDateTime cdt tz
    go [] = skipped (mkZdt before) (mkZdt after)
    go [zdt] = zdt
    go (zdt1:zdt2:[]) = ambiguous zdt1 zdt2
    go _ = error "misconfiguration: more than 2 dates returns from calDateTransitionsFor"

-- conversion

-- | Return the 'CalendarDateTime' represented by this 'ZonedDateTime'.
toCalendarDateTime :: ZonedDateTime cal -> CalendarDateTime cal
toCalendarDateTime (ZonedDateTime cdt _  _) = cdt

-- | Return the 'CalendarDate' represented by this 'ZonedDateTime'.
toCalendarDate :: ZonedDateTime cal -> CalendarDate cal
toCalendarDate (ZonedDateTime (CalendarDateTime cd _) _  _) = cd

-- | Return the 'LocalTime' represented by this 'ZonedDateTime'.
toLocalTime :: ZonedDateTime cal -> LocalTime
toLocalTime (ZonedDateTime (CalendarDateTime _ lt) _  _) = lt

-- | Return the 'Instant' represented by this 'ZonedDateTime'
toInstant :: IsCalendarDateTime cal => ZonedDateTime cal -> Instant
toInstant zdt = adjusted
  where
    offset = tiUtcOffset . zdtActiveTransition $ zdt
    d = fromSeconds $ offsetSeconds offset
    unadjusted = toUnadjustedInstant . zdtCalendarDateTime $ zdt
    adjusted = unadjusted `minus` d
-- Accessors

-- | Return a 'Bool' specifying if this 'ZonedDateTime' is currently in Daylight savings time.
inDst :: ZonedDateTime cal -> Bool
inDst (ZonedDateTime _ _ (TransitionInfo _ isInDst _)) = isInDst

-- | Return a 'String' representing the abbreviation for the TimeZone this 'ZonedDateTime' is currently in.
zoneAbbreviation :: ZonedDateTime cal -> String
zoneAbbreviation (ZonedDateTime _ _ (TransitionInfo _ _ abbr)) = abbr
