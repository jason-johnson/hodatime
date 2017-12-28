module Data.HodaTime.OffsetDateTime
(
  -- * Types
   OffsetDateTime
  -- * Constructors
  ,fromInstantWithOffset
  ,fromCalendarDateTimeWithOffset
  -- * Math
  -- * Conversion
)
where

import Data.HodaTime.OffsetDateTime.Internal
import Data.HodaTime.Instant.Internal (Instant)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..), TZIdentifier(..), TransitionInfo)
import Data.HodaTime.TimeZone.Platform (fixedOffsetZone)

-- | Create an 'OffsetDateTime' from an 'Instant' and an 'Offset'.
fromInstantWithOffset :: IsCalendarDateTime cal => Instant -> Offset -> IO (OffsetDateTime cal)
fromInstantWithOffset inst offset = do
  (tz, tInfo) <- makeFixedTimeZone offset
  return . OffsetDateTime . ZonedDateTime cdt tz $ tInfo
    where
      cdt = fromAdjustedInstant . adjustInstant secs $ inst
      secs = fromIntegral . offsetSeconds $ offset

-- | Create an 'OffsetDateTime' from a 'CalendarDateTime' and an 'Offset'.
fromCalendarDateTimeWithOffset :: CalendarDateTime cal -> Offset -> IO (OffsetDateTime cal)
fromCalendarDateTimeWithOffset cdt offset = do
  (tz, tInfo) <- makeFixedTimeZone offset
  return . OffsetDateTime . ZonedDateTime cdt tz $ tInfo

-- helper functions

makeFixedTimeZone :: Offset -> IO (TimeZone, TransitionInfo)
makeFixedTimeZone offset = do
  (utcM, calDateM, leapsM, tExprDetails, tInfo) <- fixedOffsetZone tzName (fromIntegral . offsetSeconds $ offset)
  return $ (TimeZone (Zone tzName) utcM calDateM leapsM tExprDetails, tInfo)
    where
      tzName = toStringRep offset