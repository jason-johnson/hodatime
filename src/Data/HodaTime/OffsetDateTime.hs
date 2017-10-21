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
import Data.HodaTime.Instant.Internal (Instant, add, minus)
import Data.HodaTime.Duration.Internal (fromSeconds)
import Data.HodaTime.CalendarDateTime.Internal (CalendarDateTime, IsCalendarDateTime(..))
import Data.HodaTime.ZonedDateTime.Internal (ZonedDateTime(..))
import Data.HodaTime.TimeZone.Internal (TimeZone(..), TZIdentifier(..))
import Data.HodaTime.TimeZone.Platform (fixedOffsetZone)

-- | Create an 'OffsetDateTime' from an 'Instant' and an 'Offset'.
fromInstantWithOffset :: IsCalendarDateTime cal => Instant -> Offset -> IO (OffsetDateTime cal)
fromInstantWithOffset inst offset = do
  tz <- makeFixedTimeZone offset
  return . OffsetDateTime . ZonedDateTime cdt $ tz
    where
      cdt = fromAdjustedInstant inst'
      secs = fromIntegral . offsetSeconds $ offset
      op = if secs < 0 then minus else add
      duration = fromSeconds . abs $ secs
      inst' = inst `op` duration

-- | Create an 'OffsetDateTime' from a 'CalendarDateTime' and an 'Offset'.
fromCalendarDateTimeWithOffset :: CalendarDateTime cal -> Offset -> IO (OffsetDateTime cal)
fromCalendarDateTimeWithOffset cdt offset = do
  tz <- makeFixedTimeZone offset
  return . OffsetDateTime . ZonedDateTime cdt $ tz

-- helper functions

makeFixedTimeZone :: Offset -> IO TimeZone
makeFixedTimeZone offset = do
  (utcM, calDateM, leapsM) <- fixedOffsetZone tzName (fromIntegral . offsetSeconds $ offset)
  return $ TimeZone (Zone tzName) utcM calDateM leapsM
    where
      tzName = toStringRep offset