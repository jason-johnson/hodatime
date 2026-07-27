module HodaTime.LocaleTest
(
  localeTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromMaybe)

import Data.HodaTime.Locale
import Data.HodaTime.Pattern
import Data.HodaTime.Pattern.CalendarDate (pdd, pyyyy, pMMMM', pMMM', pdddd', pMonthName)
import Data.HodaTime.Pattern.LocalTime (phh, pmm, ppp')
import Data.HodaTime.Pattern.Locale (compileDatePattern, compileTimePattern, localeDatePattern, localeTimePattern)
import Data.HodaTime.LocalTime (localTime, LocalTime)
import Data.HodaTime.CalendarDate (CalendarDate)
import qualified Data.HodaTime.Calendar.Gregorian as G

-- | A hand-built German locale, so the pattern tests are deterministic and do not depend on which locales happen to be
--   installed on the build machine.  (The AM\/PM designators are non-empty here only so the period round-trip can be
--   exercised; a real @de_DE@ leaves them empty.)
de :: Locale
de = Locale
  { localeId          = "de_DE.TEST"
  , monthNames        = ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"]
  , monthNamesShort   = ["Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"]
  , dayNames          = ["Sonntag","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag"]
  , dayNamesShort     = ["So","Mo","Di","Mi","Do","Fr","Sa"]
  , amName            = "AM"
  , pmName            = "PM"
  , rawDateFormat     = "%d.%m.%Y"
  , rawTimeFormat     = "%T"
  , rawDateTimeFormat = "%a %d %b %Y %T %Z"
  }

tuesday :: CalendarDate G.Gregorian
tuesday = fromMaybe (error "impossible") $ G.calendarDate 3 G.March 2020

mar15 :: CalendarDate G.Gregorian
mar15 = fromMaybe (error "impossible") $ G.calendarDate 15 G.March 2020

mkLtm :: Int -> Int -> LocalTime
mkLtm h m = fromMaybe (error "impossible") (localTime h m 0 0)

mkLts :: Int -> Int -> Int -> LocalTime
mkLts h m s = fromMaybe (error "impossible") (localTime h m s 0)

localeTests :: TestTree
localeTests = testGroup "Locale Tests" [patternTests, readerTests, strftimeTests]

patternTests :: TestTree
patternTests = testGroup "locale-aware patterns"
  [
     testCase "format pMMMM' is the German month name"        $ format (pMMMM' de) tuesday @?= "März"
    ,testCase "format pMMM' is the short German month name"   $ format (pMMM' de) tuesday @?= "Mär"
    ,testCase "format pdddd' is the German weekday name"      $ format (pdddd' de) tuesday @?= "Dienstag"
    ,testCase "parse German full date round-trips"           $ parse (pdd <% char ' ' <> pMMMM' de <% char ' ' <> pyyyy) "03 März 2020" @?= Just tuesday
    ,testCase "parse consumes the German weekday"            $ parse (pdddd' de <% string ", " <> pdd <% char ' ' <> pMMMM' de <% char ' ' <> pyyyy) "Dienstag, 03 März 2020" @?= Just tuesday
    ,testCase "generic pMonthName uses any name vector"      $ format (pMonthName ["M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12"]) tuesday @?= "M3"
    ,testCase "format ppp' with custom PM designator"        $ format (ppp' de) (mkLtm 15 0) @?= "PM"
    ,testCase "parse 12-hour + ppp' round-trips"             $ parse (phh <% char ':' <> pmm <% char ' ' <> ppp' de) "03:04 PM" @?= Just (mkLtm 15 4)
  ]

readerTests :: TestTree
readerTests = testGroup "reading the machine locale"
  [
     testCase "the C locale has the English month names" $ do
       loc <- localeByName "C"
       take 3 (monthNames loc) @?= ["January", "February", "March"]
       length (monthNames loc) @?= 12
    ,testCase "the C locale has seven weekday names, Sunday-first" $ do
       loc <- localeByName "C"
       head (dayNames loc) @?= "Sunday"
       length (dayNames loc) @?= 7
    ,testCase "currentLocale returns a structurally complete locale" $ do
       loc <- currentLocale
       length (monthNames loc) @?= 12
       length (monthNamesShort loc) @?= 12
       length (dayNames loc) @?= 7
       length (dayNamesShort loc) @?= 7
  ]

strftimeTests :: TestTree
strftimeTests = testGroup "strftime layout compiler"
  [
     testCase "numeric date %d.%m.%Y formats"           $ fmap (\p -> format p tuesday) (compileDatePattern de "%d.%m.%Y") @?= Just "03.03.2020"
    ,testCase "numeric date %d.%m.%Y round-trips"        $ (compileDatePattern de "%d.%m.%Y" >>= \p -> parse p "03.03.2020") @?= Just tuesday
    ,testCase "named date %A, %d %B %Y formats"          $ fmap (\p -> format p tuesday) (compileDatePattern de "%A, %d %B %Y") @?= Just "Dienstag, 03 März 2020"
    ,testCase "named date %A, %d %B %Y round-trips"      $ (compileDatePattern de "%A, %d %B %Y" >>= \p -> parse p "Dienstag, 03 März 2020") @?= Just tuesday
    ,testCase "multibyte literals (Japanese layout)"     $ fmap (\p -> format p tuesday) (compileDatePattern de "%Y年%m月%d日") @?= Just "2020年03月03日"
    ,testCase "multibyte literals round-trip"            $ (compileDatePattern de "%Y年%m月%d日" >>= \p -> parse p "2020年03月03日") @?= Just tuesday
    ,testCase "composite %F expands to %Y-%m-%d"         $ fmap (\p -> format p tuesday) (compileDatePattern de "%F") @?= Just "2020-03-03"
    ,testCase "localeDatePattern uses the locale D_FMT"  $ fmap (\p -> format p tuesday) (localeDatePattern de) @?= Just "03.03.2020"
    ,testCase "time %H:%M:%S formats"                    $ fmap (\p -> format p (mkLts 15 4 9)) (compileTimePattern de "%H:%M:%S") @?= Just "15:04:09"
    ,testCase "composite %T expands to %H:%M:%S"         $ (compileTimePattern de "%T" >>= \p -> parse p "15:04:09") @?= Just (mkLts 15 4 9)
    ,testCase "12-hour %I:%M %p formats"                 $ fmap (\p -> format p (mkLtm 15 4)) (compileTimePattern de "%I:%M %p") @?= Just "03:04 PM"
    ,testCase "12-hour %I:%M %p round-trips"             $ (compileTimePattern de "%I:%M %p" >>= \p -> parse p "03:04 PM") @?= Just (mkLtm 15 4)
    ,testCase "localeTimePattern uses the locale T_FMT"  $ fmap (\p -> format p (mkLts 15 4 9)) (localeTimePattern de) @?= Just "15:04:09"
    ,testCase "unsupported specifier is rejected"        $ (compileDatePattern de "%V" >>= \p -> parse p "x") @?= (Nothing :: Maybe (CalendarDate G.Gregorian))
    ,testCase "a time field in a date layout is rejected" $ (compileDatePattern de "%H" >>= \p -> parse p "x") @?= (Nothing :: Maybe (CalendarDate G.Gregorian))
    ,testCase "%e space-pads a single-digit day"         $ fmap (\p -> format p tuesday) (compileDatePattern de "%e.%m.%Y") @?= Just " 3.03.2020"
    ,testCase "%e leaves a two-digit day unpadded"       $ fmap (\p -> format p mar15) (compileDatePattern de "%e.%m.%Y") @?= Just "15.03.2020"
    ,testCase "%e round-trips the padded form"           $ (compileDatePattern de "%e.%m.%Y" >>= \p -> parse p " 3.03.2020") @?= Just tuesday
    ,testCase "%e also accepts the bare form"            $ (compileDatePattern de "%e.%m.%Y" >>= \p -> parse p "3.03.2020") @?= Just tuesday
    ,testCase "%l space-pads the 12-hour clock"          $ fmap (\p -> format p (mkLtm 13 24)) (compileTimePattern de "%l:%M %p") @?= Just " 1:24 PM"
    ,testCase "%l round-trips"                           $ (compileTimePattern de "%l:%M %p" >>= \p -> parse p " 1:24 PM") @?= Just (mkLtm 13 24)
  ]
