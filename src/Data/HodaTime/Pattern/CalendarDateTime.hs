{-# LANGUAGE FlexibleContexts #-}
module Data.HodaTime.Pattern.CalendarDateTime
(
  -- * Standard Patterns
   ps
  ,pf
  ,pF
  ,pg
  ,pG
  -- * Custom Patterns
  --
  -- | Use combination of `Data.HodaTime.Pattern.CalendarDate` and `Data.HodaTime.Pattern.LocalTime` patterns
)
where

import Data.HodaTime.Pattern.Internal
import Data.HodaTime.CalendarDateTime.Internal (HasDate, Month, IsCalendar)
import Data.HodaTime.LocalTime.Internal (HasLocalTime)
import Data.HodaTime.Pattern.LocalTime
import Data.HodaTime.Pattern.CalendarDate

-- d1 = maybe (error "duh") id $ on <$> localTime 1 2 3 0 <*> calendarDate 1 January 2000
-- d2 = maybe (error "duh") id $ on <$> localTime 1 2 3 0 <*> calendarDate 3 March 2020
-- format ps d1
-- format ps d2
-- parse ps "2000/March/01" :: IO (CalendarDate Gregorian)

-- | The sortable pattern, which is always "yyyy'-'MM'-'dd'T'HH':'mm':'ss". (Note: this is only truly sortable for years within the range [0-9999].)
ps :: (HasLocalTime dt, HasDate dt) => Pattern (dt -> dt) (dt -> String) String
ps = pyyyy <% char '-' <> pMM <% char '-' <> pdd <% char 'T' <> pHH <% char ':' <> pmm <% char ':' <> pss

-- | The long date pattern followed by a space, followed by the short time pattern.
pf :: (HasLocalTime (c cal), HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pf = pD <% char ' ' <> pt

-- | The full date and time pattern. This is currently "dddd, dd MMMM yyyy HH:mm:ss".
pF :: (HasLocalTime (c cal), HasDate (c cal), IsCalendar cal, Bounded (Month cal), Read (Month cal), Show (Month cal), Enum (Month cal)) => Pattern (c cal -> c cal) (c cal -> String) String
pF = pD <% char ' ' <> pT

-- | The short date pattern followed by a space, followed by the short time pattern.
pg :: (HasLocalTime dt, HasDate dt) => Pattern (dt -> dt) (dt -> String) String
pg = pd <% char ' ' <> pt

-- | The short date pattern followed by a space, followed by the long time pattern.
pG :: (HasLocalTime dt, HasDate dt) => Pattern (dt -> dt) (dt -> String) String
pG = pd <% char ' ' <> pT
