module HodaTime.LocaleTest
(
  localeTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromMaybe, isNothing)

import Data.HodaTime.Locale
import Data.HodaTime.Pattern
import Data.HodaTime.Pattern.CalendarDate (pdd, pMM, pyyyy, pMMMM', pMMM', pdddd', pMonthName, pdaySpace)
import Data.HodaTime.Pattern.LocalTime (phh, phhSpace, pmm, ppp')
import Data.HodaTime.Pattern.Locale (localeDatePattern, localeTimePattern, localeDateTimePattern, parseZonedDateTime)
import Data.HodaTime.LocalTime (localTime, LocalTime)
import Data.HodaTime.CalendarDate (CalendarDate)
import Data.HodaTime.CalendarDateTime (CalendarDateTime, at)
import Data.HodaTime.ZonedDateTime (ZonedDateTime, toCalendarDateTime, fromCalendarDateTimeStrictly)
import Data.HodaTime.TimeZone (utc)
import qualified Data.HodaTime.Calendar.Gregorian as G

tuesday :: CalendarDate G.Gregorian
tuesday = fromMaybe (error "impossible") $ G.calendarDate 3 G.March 2020

mar15 :: CalendarDate G.Gregorian
mar15 = fromMaybe (error "impossible") $ G.calendarDate 15 G.March 2020

mkLtm :: Int -> Int -> LocalTime
mkLtm h m = fromMaybe (error "impossible") (localTime h m 0 0)

mkLts :: Int -> Int -> Int -> LocalTime
mkLts h m s = fromMaybe (error "impossible") (localTime h m s 0)

-- | 15 March 2020 (a Sunday), 13:24:35 — all date and time fields distinct, for the combined-layout tests.
dt :: CalendarDateTime G.Gregorian
dt = at mar15 (mkLts 13 24 35)

localeTests :: TestTree
localeTests = testGroup "Locale Tests" [patternTests, readerTests, strftimeTests, zonedTests]

patternTests :: TestTree
patternTests = testGroup "locale-aware patterns"
  [
     testCase "format pMMMM' is the German month name"        $ format (pMMMM' deDE) tuesday @?= "März"
    ,testCase "format pMMM' is the short German month name"   $ format (pMMM' deDE) tuesday @?= "Mär"
    ,testCase "format pMMMM' is the Japanese month name"      $ format (pMMMM' jaJP) tuesday @?= "3月"
    ,testCase "format pdddd' is the German weekday name"      $ format (pdddd' deDE) tuesday @?= "Dienstag"
    ,testCase "parse German full date round-trips"           $ parse (pdd <% char ' ' <> pMMMM' deDE <% char ' ' <> pyyyy) "03 März 2020" @?= Just tuesday
    ,testCase "parse consumes the German weekday"            $ parse (pdddd' deDE <% string ", " <> pdd <% char ' ' <> pMMMM' deDE <% char ' ' <> pyyyy) "Dienstag, 03 März 2020" @?= Just tuesday
    ,testCase "generic pMonthName uses any name vector"      $ format (pMonthName ["M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12"]) tuesday @?= "M3"
    ,testCase "format ppp' is the PM designator"             $ format (ppp' enUS) (mkLtm 15 0) @?= "PM"
    ,testCase "parse 12-hour + ppp' round-trips"             $ parse (phh <% char ':' <> pmm <% char ' ' <> ppp' enUS) "03:04 PM" @?= Just (mkLtm 15 4)
    ,testCase "pdaySpace pads a single-digit day"            $ format pdaySpace tuesday @?= " 3"
    ,testCase "pdaySpace leaves a two-digit day"             $ format pdaySpace mar15 @?= "15"
    ,testCase "pdaySpace parses the padded form"             $ parse (pdaySpace <% char '.' <> pMM <% char '.' <> pyyyy) " 3.03.2020" @?= Just tuesday
    ,testCase "pdaySpace parses the bare form"               $ parse (pdaySpace <% char '.' <> pMM <% char '.' <> pyyyy) "3.03.2020" @?= Just tuesday
    ,testCase "phhSpace pads the 12-hour clock"              $ format (phhSpace <% char ':' <> pmm <% char ' ' <> ppp' enUS) (mkLtm 13 24) @?= " 1:24 PM"
    ,testCase "phhSpace round-trips"                         $ parse (phhSpace <% char ':' <> pmm <% char ' ' <> ppp' enUS) " 1:24 PM" @?= Just (mkLtm 13 24)
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
     testCase "US date is month-first (%m/%d/%Y)"         $ fmap (\p -> format p mar15) (localeDatePattern enUS) @?= Just "03/15/2020"
    ,testCase "US date round-trips"                       $ (localeDatePattern enUS >>= \p -> parse p "03/15/2020") @?= Just mar15
    ,testCase "German date is day-first (%d.%m.%Y)"       $ fmap (\p -> format p mar15) (localeDatePattern deDE) @?= Just "15.03.2020"
    ,testCase "German date round-trips"                   $ (localeDatePattern deDE >>= \p -> parse p "15.03.2020") @?= Just mar15
    ,testCase "Japanese date has multibyte separators"    $ fmap (\p -> format p mar15) (localeDatePattern jaJP) @?= Just "2020年03月15日"
    ,testCase "Japanese date round-trips"                 $ (localeDatePattern jaJP >>= \p -> parse p "2020年03月15日") @?= Just mar15
    ,testCase "US time is 12-hour (%r)"                   $ fmap (\p -> format p (mkLts 13 24 35)) (localeTimePattern enUS) @?= Just "01:24:35 PM"
    ,testCase "US time round-trips"                       $ (localeTimePattern enUS >>= \p -> parse p "01:24:35 PM") @?= Just (mkLts 13 24 35)
    ,testCase "German time is 24-hour (%T)"               $ fmap (\p -> format p (mkLts 13 24 35)) (localeTimePattern deDE) @?= Just "13:24:35"
    ,testCase "German time round-trips"                   $ (localeTimePattern deDE >>= \p -> parse p "13:24:35") @?= Just (mkLts 13 24 35)
    ,testCase "Japanese time has multibyte separators"    $ fmap (\p -> format p (mkLts 13 24 35)) (localeTimePattern jaJP) @?= Just "13時24分35秒"
    ,testCase "Japanese time round-trips"                 $ (localeTimePattern jaJP >>= \p -> parse p "13時24分35秒") @?= Just (mkLts 13 24 35)
    ,testCase "German date+time drops the zone"           $ fmap (\p -> format p dt) (localeDateTimePattern deDE) @?= Just "So 15 Mär 2020 13:24:35"
    ,testCase "German date+time round-trips"              $ (localeDateTimePattern deDE >>= \p -> parse p "So 15 Mär 2020 13:24:35") @?= Just dt
    ,testCase "US date+time is 12-hour, zone dropped"     $ fmap (\p -> format p dt) (localeDateTimePattern enUS) @?= Just "Sun 15 Mar 2020 01:24:35 PM"
    ,testCase "US date+time round-trips"                  $ (localeDateTimePattern enUS >>= \p -> parse p "Sun 15 Mar 2020 01:24:35 PM") @?= Just dt
    ,testCase "Japanese date+time (no zone in layout)"    $ fmap (\p -> format p dt) (localeDateTimePattern jaJP) @?= Just "2020年03月15日 13時24分35秒"
    ,testCase "Japanese date+time round-trips"           $ (localeDateTimePattern jaJP >>= \p -> parse p "2020年03月15日 13時24分35秒") @?= Just dt
  ]

zonedTests :: TestTree
zonedTests = testGroup "locale ZonedDateTime parsing"
  [
     testCase "reads the local time from a zoned layout" $ do
       tz <- utc
       zdt <- parseZonedDateTime (const (pure tz)) fromCalendarDateTimeStrictly deDE "So 15 Mär 2020 13:24:35 UTC" :: IO (ZonedDateTime G.Gregorian)
       toCalendarDateTime zdt @?= dt
    ,testCase "rejects a zoneless (Japanese) layout"     $ isNothing (parseZonedDateTime (const Nothing) fromCalendarDateTimeStrictly jaJP "irrelevant" :: Maybe (ZonedDateTime G.Gregorian)) @?= True
  ]
