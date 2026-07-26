module HodaTime.PatternTest
(
  patternTests
)
where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic as QCM
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadThrow)
import Data.Semigroup ((<>))
import Test.Tasty.HUnit
import HodaTime.Util
import Data.HodaTime.LocalTime (localTime)
import Data.HodaTime.CalendarDateTime (at, CalendarDateTime)
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.Pattern
import Data.HodaTime.Pattern.CalendarDate
import Data.HodaTime.Pattern.CalendarDateTime
import Data.HodaTime.Pattern.LocalTime
import Data.HodaTime.Pattern.Instant
import Data.HodaTime.Pattern.Offset
import Data.HodaTime.Pattern.OffsetDateTime
import Data.HodaTime.Pattern.Duration
import Data.HodaTime.Pattern.ZonedDateTime (pZonedDateTime, parseZonedDateTime)
import Data.HodaTime.Instant (fromSecondsSinceUnixEpoch)
import Data.HodaTime.TimeZone (utc)
import Data.HodaTime.ZonedDateTime (ZonedDateTime, fromInstant, fromCalendarDateTimeStrictly)
import Data.HodaTime.Offset (Offset, fromHours, fromMinutes, fromSeconds, empty)
import Data.HodaTime.OffsetDateTime (fromCalendarDateTimeWithOffset)
import qualified Data.HodaTime.Duration as Dur (fromStandardDays, fromSeconds, fromNanoseconds, add)
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

-- instance MonadThrow (QCM.PropertyM IO)

patternTests :: TestTree
patternTests = testGroup "Pattern Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [ calDateTimeProps, calDateProps, localTimeProps, instantProps, offsetProps, offsetDateTimeProps, durationProps ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
     testCase "format pddd is the abbreviated weekday name" $ format pddd tuesday @?= "Tue"
    ,testCase "format pdddd is the full weekday name"       $ format pdddd tuesday @?= "Tuesday"
    ,testCase "format pD includes the weekday"              $ format pD tuesday @?= "Tuesday, 03 March 2020"
    ,testCase "format pMMM is the abbreviated month name"   $ format pMMM tuesday @?= "Mar"
    ,testCase "parse pMMM round-trips a date"               $ parse (pdd <% char ' ' <> pMMM <% char ' ' <> pyyyy) "03 Mar 2020" @?= Just tuesday
    ,testCase "format pmonthDay is MMMM dd"                 $ format pmonthDay tuesday @?= "March 03"
    ,testCase "format pyearMonth is yyyy MMMM"              $ format pyearMonth tuesday @?= "2020 March"
    ,testCase "format phh folds 15:00 to 03"                $ format phh (mkLt 15) @?= "03"
    ,testCase "format phh folds 00:00 to 12"                $ format phh (mkLt 0) @?= "12"
    ,testCase "format phh folds 12:00 to 12"                $ format phh (mkLt 12) @?= "12"
    ,testCase "format pp is P in the afternoon"             $ format pp (mkLt 15) @?= "P"
    ,testCase "format pp is A in the morning"               $ format pp (mkLt 3) @?= "A"
    ,testCase "format ppp is PM at noon"                    $ format ppp (mkLt 12) @?= "PM"
    ,testCase "format ppp is AM at midnight"                $ format ppp (mkLt 0) @?= "AM"
    ,testCase "format 12-hour+AM/PM at 15:04"               $ format hhmmp (mkLtm 15 4) @?= "03:04 PM"
    ,testCase "parse 12-hour+AM/PM 03:04 PM is 15:04"       $ parse hhmmp "03:04 PM" @?= Just (mkLtm 15 4)
    ,testCase "parse 12-hour+AM/PM 12:00 AM is midnight"    $ parse hhmmp "12:00 AM" @?= Just (mkLtm 0 0)
    ,testCase "parse 12-hour+AM/PM 12:00 PM is noon"        $ parse hhmmp "12:00 PM" @?= Just (mkLtm 12 0)
    ,testCase "format pfrac 3 truncates to milliseconds"    $ format (pfrac 3) (mkLtn 123456789) @?= "123"
    ,testCase "format pfrac 6 truncates to microseconds"    $ format (pfrac 6) (mkLtn 123456789) @?= "123456"
    ,testCase "format pfrac 9 shows full nanoseconds"       $ format (pfrac 9) (mkLtn 123456789) @?= "123456789"
    ,testCase "format pfrac 3 zero-pads"                    $ format (pfrac 3) (mkLtn 7000000) @?= "007"
    ,testCase "parse pfrac 3 scales up to nanoseconds"      $ parse (pfrac 3) "123" @?= Just (mkLtn 123000000)
    ,testCase "parse pfrac 9 reads full nanoseconds"        $ parse (pfrac 9) "123456789" @?= Just (mkLtn 123456789)    -- width parameter: width 1 is the no-padding case (unpadded format, variable-width parse); width >= 2 is fixed padded.
    ,testCase "format pday 1 has no leading zero"           $ format (pday 1) tuesday @?= "3"
    ,testCase "format pday 2 zero-pads (== pdd)"            $ format (pday 2) tuesday @?= format pdd tuesday
    ,testCase "format phour 1 has no leading zero"          $ format (phour 1) (mkLt 3) @?= "3"
    ,testCase "format no-padding date"                     $ format (pyear 1 <% char '-' <> pmonthNum 1 <% char '-' <> pday 1) tuesday @?= "2020-3-3"
    ,testCase "parse no-padding date round-trips"          $ parse (pyear 1 <% char '-' <> pmonthNum 1 <% char '-' <> pday 1) "2020-3-3" @?= Just tuesday
    ,testCase "no-padding parse also accepts padded input" $ parse (pyear 1 <% char '-' <> pmonthNum 1 <% char '-' <> pday 1) "2020-03-03" @?= Just tuesday
    -- pyy is the interpreting two-digit year (Noda's yy): format emits year mod 100; parse infers the century from the
    -- parse template (default year 2000).  Contrast pyear, which is strict/absolute and never truncates.
    ,testCase "format pyy emits the last two digits"       $ format pyy tuesday @?= "20"
    ,testCase "format pyear 2 does not truncate (strict)"  $ format (pyear 2) tuesday @?= "2020"
    ,testCase "parse pyy 20 infers 2020"                   $ parse (pyy <% char '-' <> pMM <% char '-' <> pdd) "20-03-03" @?= Just tuesday
    ,testCase "parse pyy 99 infers 1999"                   $ parse (pyy <% char '-' <> pMM <% char '-' <> pdd) "99-03-03" @?= Just (mar3 1999)
    ,testCase "parse pyy 00 infers 2000"                   $ parse (pyy <% char '-' <> pMM <% char '-' <> pdd) "00-03-03" @?= Just (mar3 2000)    ,testCase "format pR is the ISO date"                   $ format pR tuesday @?= "2020-03-03"
    -- setter-application order independence: <> applies the right operand's setter first, so verify that
    -- reordering composed fields (independent lenses, and the hour-sharing phh/ppp) yields the same result.
    ,testCase "date field order is independent"             $ parse (pyyyy <% char '-' <> pMM <% char '-' <> pdd) "2020-03-03" @?= Just tuesday
    ,testCase "date field order is independent (reversed)"  $ parse (pdd <% char '-' <> pMM <% char '-' <> pyyyy) "03-03-2020" @?= Just tuesday
    ,testCase "phh before ppp parses PM"                    $ parse (phh <% char ' ' <> ppp) "03 PM" @?= Just (mkLtm 15 0)
    ,testCase "ppp before phh parses PM (reversed order)"   $ parse (ppp <% char ' ' <> phh) "PM 03" @?= Just (mkLtm 15 0)
    ,testCase "phh before ppp parses AM at midnight"        $ parse (phh <% char ' ' <> ppp) "12 AM" @?= Just (mkLtm 0 0)
    ,testCase "ppp before phh parses AM at midnight (rev)"  $ parse (ppp <% char ' ' <> phh) "AM 12" @?= Just (mkLtm 0 0)
    ,testCase "format pInstant at the Unix epoch"          $ format pInstant (fromSecondsSinceUnixEpoch 0) @?= "1970-01-01T00:00:00Z"
    ,testCase "parse pInstant at the Unix epoch"           $ parse pInstant "1970-01-01T00:00:00Z" @?= Just (fromSecondsSinceUnixEpoch 0)
    ,testCase "format pInstantNano at the Unix epoch"      $ format pInstantNano (fromSecondsSinceUnixEpoch 0) @?= "1970-01-01T00:00:00.000000000Z"
    ,testCase "format pOffset +02:00"                      $ format pOffset (fromHours 2) @?= "+02:00"
    ,testCase "format pOffset -05:30"                      $ format pOffset (fromMinutes (-330)) @?= "-05:30"
    ,testCase "format pOffset UTC is +00:00"               $ format pOffset empty @?= "+00:00"
    ,testCase "format pOffsetFull -05:30:45"               $ format pOffsetFull (fromSeconds (-19845 :: Int)) @?= "-05:30:45"
    ,testCase "parse pOffset +02:00"                       $ parse pOffset "+02:00" @?= Just (fromHours 2)
    ,testCase "parse pOffset -05:30"                       $ parse pOffset "-05:30" @?= Just (fromMinutes (-330))
    ,testCase "format pOffsetZ UTC is Z"                   $ format pOffsetZ empty @?= "Z"
    ,testCase "format pOffsetZ +02:00"                     $ format pOffsetZ (fromHours 2) @?= "+02:00"
    ,testCase "parse pOffsetZ Z is UTC"                    $ parse pOffsetZ "Z" @?= Just empty
    ,testCase "parse pOffsetZ +02:00"                      $ parse pOffsetZ "+02:00" @?= Just (fromHours 2)
    ,testCase "format pOffsetDateTime +02:00"              $ format pOffsetDateTime odtPos @?= "2024-04-23T09:00:00+02:00"
    ,testCase "parse pOffsetDateTime +02:00"               $ parse pOffsetDateTime "2024-04-23T09:00:00+02:00" @?= Just odtPos
    ,testCase "format pOffsetDateTime -05:30"              $ format pOffsetDateTime odtNeg @?= "2024-04-23T09:00:00-05:30"
    ,testCase "format pDuration 1 day 30s"                 $ format pDuration dur1d30 @?= "1:00:00:30"
    ,testCase "format pDuration -30s"                      $ format pDuration (Dur.fromSeconds (-30)) @?= "-0:00:00:30"
    ,testCase "parse pDuration 1:00:00:30"                 $ parse pDuration "1:00:00:30" @?= Just dur1d30
    ,testCase "format pDurationNano fraction"              $ format pDurationNano (Dur.fromNanoseconds 123456789) @?= "0:00:00:00.123456789"
    ,testCase "format pZonedDateTime at the UTC epoch"     $ do
        tz <- utc
        let zdt = fromInstant (fromSecondsSinceUnixEpoch 0) tz :: ZonedDateTime G.Gregorian
        format pZonedDateTime zdt @?= "1970-01-01T00:00:00 UTC"
    ,testCase "parseZonedDateTime resolves and round-trips" $ do
        tz <- utc
        let zdt0 = fromInstant (fromSecondsSinceUnixEpoch 0) tz :: ZonedDateTime G.Gregorian
            txt = format pZonedDateTime zdt0
        zdt1 <- parseZonedDateTime (const (pure tz)) fromCalendarDateTimeStrictly txt :: IO (ZonedDateTime G.Gregorian)
        format pZonedDateTime zdt1 @?= txt
        zdt2 <- parseZonedDateTime (const (pure tz)) fromCalendarDateTimeStrictly "2024-04-23T09:00:00 UTC" :: IO (ZonedDateTime G.Gregorian)
        format pZonedDateTime zdt2 @?= "2024-04-23T09:00:00 UTC"
  ]
  where
    tuesday = fromMaybe (error "impossible") $ G.calendarDate 3 G.March 2020
    mar3 y = fromMaybe (error "impossible") $ G.calendarDate 3 G.March y
    hhmmp = phh <% char ':' <> pmm <% char ' ' <> ppp
    mkLt h = mkLtm h 0
    mkLtm h m = fromMaybe (error "impossible") (localTime h m 0 0)
    mkLtn ns = fromMaybe (error "impossible") (localTime 0 0 0 ns)
    apr23_0900 = fromMaybe (error "impossible") $ at <$> G.calendarDate 23 G.April 2024 <*> localTime 9 0 0 0
    odtPos = fromCalendarDateTimeWithOffset apr23_0900 (fromHours 2)
    odtNeg = fromCalendarDateTimeWithOffset apr23_0900 (fromMinutes (-330))
    dur1d30 = Dur.fromStandardDays 1 `Dur.add` Dur.fromSeconds 30

-- properties

calDateTimeProps :: TestTree
calDateTimeProps = testGroup "CalendarDateTime conversion"
  [
     QC.testProperty "format ps CalendarDateTime -> parse ps CalendarDateTime == id" $ testCdtFormatToParseIdentity ps True
    ,QC.testProperty "format pf CalendarDateTime -> parse pf CalendarDateTime == id" $ testCdtFormatToParseIdentity pf False
    ,QC.testProperty "format pF CalendarDateTime -> parse pF CalendarDateTime == id" $ testCdtFormatToParseIdentity pF True
    ,QC.testProperty "format pg CalendarDateTime -> parse pg CalendarDateTime == id" $ testCdtFormatToParseIdentity pg False
    ,QC.testProperty "format pG CalendarDateTime -> parse pG CalendarDateTime == id" $ testCdtFormatToParseIdentity pG True
    ,QC.testProperty "format po CalendarDateTime -> parse po CalendarDateTime == id" $ testCdtFormatToParseIdentity po True
    ,QC.testProperty "format custom CalendarDateTime -> parse custom CalendarDateTime == id" $ testCdtFormatToParseIdentity (pyyyy <% char '/' <> pMMMM <% char '/' <> pdd <% char ' ' <> pHH <% char ':' <> pmm <% char ':' <> pss) True
  ]
  where
    seconds True s = s
    seconds False _ = 0
    testCdtFormatToParseIdentity pat useSeconds (RandomStandardDate y mon d) (RandomTime h m s) = monadicIO $ do
      let cdt = fromMaybe (error "impossible") $ at <$> G.calendarDate d mon y <*> localTime h m (seconds useSeconds s) 0
      cdt' <- run $ parse pat $ format pat cdt
      QCM.assert $ cdt == cdt'

calDateProps :: TestTree
calDateProps = testGroup "CalendarDate conversion"
  [
     QC.testProperty "format pd CalendarDate -> parse pd CalendarDate == id" $ testCdFormatToParseIdentity pd
    ,QC.testProperty "format pD CalendarDate -> parse pD CalendarDate == id" $ testCdFormatToParseIdentity pD
    ,QC.testProperty "format pR CalendarDate -> parse pR CalendarDate == id" $ testCdFormatToParseIdentity pR
    ,QC.testProperty "format custom CalendarDate -> parse custom CalendarDate == id" $ testCdFormatToParseIdentity (pyyyy <% char '/' <> pMMMM <% char '/' <> pdd)
  ]
  where
    testCdFormatToParseIdentity pat (RandomStandardDate y mon d) = monadicIO $ do
      let cd = fromMaybe (error "impossible") $ G.calendarDate d mon y
      cd' <- run $ parse pat $ format pat cd
      QCM.assert $ cd == cd'
localTimeProps :: TestTree
localTimeProps = testGroup "LocalTime conversion"
  [
     QC.testProperty "format pt LocalTime -> parse pt LocalTime == id" $ testLtFormatToParseIdentity pt False
    ,QC.testProperty "format pT LocalTime -> parse pT LocalTime == id" $ testLtFormatToParseIdentity pT True
    ,QC.testProperty "format custom LocalTime -> parse custom LocalTime == id" $ testLtFormatToParseIdentity (pHH <% char ':' <> pmm <% char ':' <> pss) True
    ,QC.testProperty "format 12-hour+AM/PM LocalTime -> parse == id" $ testLtFormatToParseIdentity (phh <% char ':' <> pmm <% char ' ' <> ppp) False
    ,QC.testProperty "format pr LocalTime -> parse pr LocalTime == id" $ testLtFormatToParseIdentity pr True
    ,QC.testProperty "format pfrac 9 LocalTime -> parse == id (nanoseconds)" testFracRoundTrip
  ]
  where
    seconds True s = s
    seconds False _ = 0
    testLtFormatToParseIdentity pat useSeconds (RandomTime h m s) = monadicIO $ do
      let lt = fromMaybe (error "impossible") $ localTime h m (seconds useSeconds s) 0
      lt' <- run $ parse pat $ format pat lt
      QCM.assert $ lt == lt'
    testFracRoundTrip (NonNegative ns0) = monadicIO $ do
      let ns = ns0 `mod` 1000000000
      let lt = fromMaybe (error "impossible") $ localTime 0 0 0 ns
      lt' <- run $ parse (pfrac 9) $ format (pfrac 9) lt
      QCM.assert $ lt == lt'

instantProps :: TestTree
instantProps = testGroup "Instant conversion"
  [
     QC.testProperty "format pInstant Instant -> parse pInstant Instant == id" $ testInstantRoundTrip pInstant
    ,QC.testProperty "format pInstantNano Instant -> parse pInstantNano Instant == id" $ testInstantRoundTrip pInstantNano
  ]
  where
    testInstantRoundTrip pat (NonNegative s0) = monadicIO $ do
      let s = s0 `mod` 253402300800   -- keep within 4-digit Gregorian years (1970-9999)
      let inst = fromSecondsSinceUnixEpoch s
      inst' <- run $ parse pat $ format pat inst
      QCM.assert $ inst == inst'

offsetProps :: TestTree
offsetProps = testGroup "Offset conversion"
  [
     QC.testProperty "format pOffset Offset -> parse pOffset == id (whole minutes)" $ testOffsetRoundTrip pOffset (fromMinutes :: Int -> Offset)
    ,QC.testProperty "format pOffsetFull Offset -> parse pOffsetFull == id" $ testOffsetRoundTrip pOffsetFull (fromSeconds :: Int -> Offset)
  ]
  where
    testOffsetRoundTrip pat mk n = monadicIO $ do
      let o = mk n
      o' <- run $ parse pat $ format pat o
      QCM.assert $ o == o'

offsetDateTimeProps :: TestTree
offsetDateTimeProps = testGroup "OffsetDateTime conversion"
  [
     QC.testProperty "format pOffsetDateTime -> parse pOffsetDateTime == id" testOdtRoundTrip
  ]
  where
    testOdtRoundTrip (RandomStandardDate y mon d) (RandomTime h m s) (RandomOffset oh om _) = monadicIO $ do
      let cdt = fromMaybe (error "impossible") $ at <$> G.calendarDate d mon y <*> localTime h m s 0
          off = fromMinutes (oh * 60 + om)
          odt = fromCalendarDateTimeWithOffset cdt off
      odt' <- run $ parse pOffsetDateTime $ format pOffsetDateTime odt
      QCM.assert $ odt == odt'

durationProps :: TestTree
durationProps = testGroup "Duration conversion"
  [
     QC.testProperty "format pDuration -> parse pDuration == id" $ testDurRoundTrip pDuration Dur.fromSeconds 100000000000
    ,QC.testProperty "format pDurationNano -> parse pDurationNano == id" $ testDurRoundTrip pDurationNano Dur.fromNanoseconds 1000000000000000
  ]
  where
    testDurRoundTrip pat mk bound n = monadicIO $ do
      let dur = mk (n `rem` bound)   -- rem keeps the sign of n, so negative durations are exercised too
      dur' <- run $ parse pat $ format pat dur
      QCM.assert $ dur == dur'