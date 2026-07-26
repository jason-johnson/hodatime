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

mkLtm :: Int -> Int -> LocalTime
mkLtm h m = fromMaybe (error "impossible") (localTime h m 0 0)

localeTests :: TestTree
localeTests = testGroup "Locale Tests" [patternTests, readerTests]

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
