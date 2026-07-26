module Data.HodaTime.Locale.Internal
(
  Locale(..)
)
where

-- | The culture-specific names used when formatting and parsing dates and times, as read from the operating system's
--   locale database (see "Data.HodaTime.Locale").
--
--   The month and weekday name lists describe the /Gregorian/ calendar, which is all the operating system's @LC_TIME@
--   category knows about.  The lists are ordered to line up directly with the calendar enumerations: 'monthNames' is
--   January-first (indexed by @fromEnum@ of the month) and 'dayNames' is Sunday-first (indexed by @fromEnum@ of the
--   'Data.HodaTime.CalendarDateTime.Internal.DayOfWeek', which also starts at Sunday).
--
--   The @raw*Format@ fields hold the operating system's layout strings (POSIX @D_FMT@ \/ @T_FMT@ \/ @D_T_FMT@)
--   verbatim.  They are captured for completeness but are not yet interpreted into patterns; locale-driven layout is a
--   later phase.
data Locale = Locale
  { localeId          :: String     -- ^ the identifier this locale was loaded from (e.g. @\"de_DE.UTF-8\"@)
  , monthNames        :: [String]   -- ^ full month names, January-first (12 entries for the Gregorian calendar)
  , monthNamesShort   :: [String]   -- ^ abbreviated month names, January-first
  , dayNames          :: [String]   -- ^ full weekday names, Sunday-first (7 entries)
  , dayNamesShort     :: [String]   -- ^ abbreviated weekday names, Sunday-first
  , amName            :: String     -- ^ the AM designator
  , pmName            :: String     -- ^ the PM designator
  , rawDateFormat     :: String     -- ^ the raw POSIX @D_FMT@ layout string (not yet interpreted)
  , rawTimeFormat     :: String     -- ^ the raw POSIX @T_FMT@ layout string (not yet interpreted)
  , rawDateTimeFormat :: String     -- ^ the raw POSIX @D_T_FMT@ layout string (not yet interpreted)
  }
  deriving (Eq, Show)
