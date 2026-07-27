module Data.HodaTime.Locale.Internal
(
  Locale(..)
 ,enUS
 ,deDE
 ,jaJP
 ,windowsPictureToStrftime
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
--   The @raw*Format@ fields hold the operating system's own layout strings as POSIX @strftime@ strings, consumed by
--   @localeDatePattern@ and friends in "Data.HodaTime.Pattern.Locale".  On POSIX they come straight from @D_FMT@ \/
--   @T_FMT@ \/ @D_T_FMT@; on Windows they are translated from the Windows /picture/ strings (e.g. @dd.MM.yyyy@) by
--   'windowsPictureToStrftime'.
data Locale = Locale
  { localeId          :: String     -- ^ the identifier this locale was loaded from (e.g. @\"de_DE.UTF-8\"@)
  , monthNames        :: [String]   -- ^ full month names, January-first (12 entries for the Gregorian calendar)
  , monthNamesShort   :: [String]   -- ^ abbreviated month names, January-first
  , dayNames          :: [String]   -- ^ full weekday names, Sunday-first (7 entries)
  , dayNamesShort     :: [String]   -- ^ abbreviated weekday names, Sunday-first
  , amName            :: String     -- ^ the AM designator
  , pmName            :: String     -- ^ the PM designator
  , rawDateFormat     :: String     -- ^ the date layout as a POSIX @strftime@ string (from @D_FMT@, or translated on Windows)
  , rawTimeFormat     :: String     -- ^ the time layout as a POSIX @strftime@ string (from @T_FMT@, or translated on Windows)
  , rawDateTimeFormat :: String     -- ^ the combined layout as a POSIX @strftime@ string (from @D_T_FMT@, or translated on Windows)
  }
  deriving (Eq, Show)

-- | Translate a Windows date\/time /picture/ string (as returned by @GetLocaleInfoEx@, e.g. @dd.MM.yyyy@) into the
--   equivalent POSIX @strftime@ string, so the locale-layout patterns can consume a Windows-read locale.  Recognised
--   fields: @d@\/@dd@ (day), @ddd@\/@dddd@ (weekday name), @M@\/@MM@ (month), @MMM@\/@MMMM@ (month name), @yy@ (2-digit
--   year) and @yyyy@+ (full year), @h@\/@hh@ (12-hour), @H@\/@HH@ (24-hour), @m@\/@mm@ (minute), @s@\/@ss@ (second),
--   @t@\/@tt@ (AM\/PM).  Text inside single quotes is literal (@''@ is a literal quote) and any other character is
--   copied through (with @%@ escaped as @%%@).
--
--   NOTE: Windows' no-pad @d@\/@M@ have no exact @strftime@ counterpart in the supported set, so single @d@ maps to the
--   space-padded @%e@ and single @M@ to @%m@ (approximations); the common two-digit @dd@\/@MM@ are exact.
windowsPictureToStrftime :: String -> String
windowsPictureToStrftime [] = []
windowsPictureToStrftime ('\'' : rest) = winLiteral rest
windowsPictureToStrftime (c : rest)
  | c `elem` "dMyHhmst" = winField c (1 + length same) ++ windowsPictureToStrftime rest'
  | c == '%'            = '%' : '%' : windowsPictureToStrftime rest
  | otherwise           = c : windowsPictureToStrftime rest
  where
    (same, rest') = span (== c) rest

-- | Map a Windows field letter and its repeat count to a @strftime@ specifier.
winField :: Char -> Int -> String
winField 'd' n | n >= 4 = "%A" | n == 3 = "%a" | n == 2 = "%d" | otherwise = "%e"
winField 'M' n | n >= 4 = "%B" | n == 3 = "%b" | otherwise = "%m"
winField 'y' n | n >= 3 = "%Y" | otherwise = "%y"
winField 'H' _ = "%H"
winField 'h' _ = "%I"
winField 'm' _ = "%M"
winField 's' _ = "%S"
winField 't' _ = "%p"
winField _   _ = ""

-- | Continue a Windows picture inside a single-quoted literal run.
winLiteral :: String -> String
winLiteral [] = []
winLiteral ('\'' : '\'' : rest) = '\'' : winLiteral rest
winLiteral ('\'' : rest)        = windowsPictureToStrftime rest
winLiteral (c : rest)
  | c == '%'  = '%' : '%' : winLiteral rest
  | otherwise = c : winLiteral rest

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
