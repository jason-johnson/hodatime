module Data.HodaTime.Locale.Internal
(
  Locale(..)
 ,enUS
 ,deDE
 ,jaJP
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
--   The @raw*Format@ fields hold the operating system's own layout strings, consumed by @localeDatePattern@ and friends
--   in "Data.HodaTime.Pattern.Locale".  On POSIX these are @strftime@ strings (@D_FMT@ \/ @T_FMT@ \/ @D_T_FMT@); on
--   Windows they are Windows /picture/ strings (e.g. @dd.MM.yyyy@), which those pattern functions do not yet understand
--   (a @strftime@ translation is a follow-up).
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

-- | A built-in United States English locale (@en_US@), offered so patterns can be produced without reading the machine.
--   Mirrors the @en_US@ @LC_TIME@ data: month-first dates and a 12-hour @%r@ time.
enUS :: Locale
enUS = Locale
  { localeId          = "en_US"
  , monthNames        = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  , monthNamesShort   = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
  , dayNames          = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
  , dayNamesShort     = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]
  , amName            = "AM"
  , pmName            = "PM"
  , rawDateFormat     = "%m/%d/%Y"
  , rawTimeFormat     = "%r"
  , rawDateTimeFormat = "%a %d %b %Y %r %Z"
  }

-- | A built-in German locale (@de_DE@): day-first dates, 24-hour time, and \(as on a real @de_DE@\) /empty/ AM\/PM
--   designators.
deDE :: Locale
deDE = Locale
  { localeId          = "de_DE"
  , monthNames        = ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"]
  , monthNamesShort   = ["Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"]
  , dayNames          = ["Sonntag","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag"]
  , dayNamesShort     = ["So","Mo","Di","Mi","Do","Fr","Sa"]
  , amName            = ""
  , pmName            = ""
  , rawDateFormat     = "%d.%m.%Y"
  , rawTimeFormat     = "%T"
  , rawDateTimeFormat = "%a %d %b %Y %T %Z"
  }

-- | A built-in Japanese locale (@ja_JP@): @年\/月\/日@-punctuated numeric dates and @午前\/午後@ AM\/PM designators.
jaJP :: Locale
jaJP = Locale
  { localeId          = "ja_JP"
  , monthNames        = ["1月","2月","3月","4月","5月","6月","7月","8月","9月","10月","11月","12月"]
  , monthNamesShort   = ["1月","2月","3月","4月","5月","6月","7月","8月","9月","10月","11月","12月"]
  , dayNames          = ["日曜日","月曜日","火曜日","水曜日","木曜日","金曜日","土曜日"]
  , dayNamesShort     = ["日","月","火","水","木","金","土"]
  , amName            = "午前"
  , pmName            = "午後"
  , rawDateFormat     = "%Y年%m月%d日"
  , rawTimeFormat     = "%H時%M分%S秒"
  , rawDateTimeFormat = "%Y年%m月%d日 %H時%M分%S秒"
  }
