module HodaTime.Calendar.GregorianTest
(
  gregorianTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Maybe (fromJust, catMaybes)
import Data.Time.Calendar (fromGregorianValid, toGregorian)

import HodaTime.Util
import Data.HodaTime.CalendarDate (day, monthl, month, year, next, previous, dayOfWeek, DayNth(..))
import Data.HodaTime.Calendar.Gregorian (calendarDate, fromNthDay, Month(..), DayOfWeek(..))
import qualified Data.HodaTime.Calendar.Gregorian as G
import qualified Data.HodaTime.Calendar.Iso as Iso

gregorianTests :: TestTree
gregorianTests = testGroup "Gregorian Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [constructorProps, lensProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [constructorUnits, lensUnits]

constructorProps :: TestTree
constructorProps = testGroup "Constructor"
  [
    QC.testProperty "same dates as Data.Time" $ testConstructor
  ]
    where
      areSame Nothing Nothing = True
      areSame (Just hdate) (Just date) =
        let
          (ty, tm, tday) = toGregorian date
        in get day hdate == tday && (convertMonth . month $ hdate) == tm && get year hdate == (fromIntegral ty)
      areSame _ _ = False
      convertMonth = succ . fromEnum
      testConstructor (Positive y) m (Positive d) = areSame (calendarDate d m y') (fromGregorianValid (fromIntegral y') (convertMonth m) d)
        where
          y' = 1900 + y

lensProps :: TestTree
lensProps = testGroup "Lens"
  [
     QC.testProperty "first day not changed by month math" $ testMonthAdd 1
    ,QC.testProperty "mid day not changed by month math" $ testMonthAdd 15
    ,QC.testProperty "dayOfWeek . next n dow $ date == dow" $ testNextDoW
    ,QC.testProperty "next n (dayOfWeek date) date == modify (+ n * 7) day date" $ testDirection next (+)
    ,QC.testProperty "previous n (dayOfWeek date) date == modify (- n * 7) day date" $ testDirection previous $ flip (-)
    ,QC.testProperty "next 1 dow date < modify (+ 8) day date" $ testDirectionRange next (<) (+)
    ,QC.testProperty "previous 1 dow date > modify (- 8) day date" $ testDirectionRange previous (>) $ flip (-)
  ]
  where
    mkcd d m = fromJust . calendarDate d m
    testMonthAdd d (CycleYear y) m add = get day (modify (+ add) monthl $ mkcd d m (y + 1900)) == d  -- NOTE: We fix the year so we don't run out of tests
    testNextDoW dow (Positive n) = (dayOfWeek . next n dow $ epochDay) == dow
    testDirection dir adjust (Positive n) = dir n (dayOfWeek epochDay) epochDay == modify (adjust $ n * 7) day epochDay
    testDirectionRange dir gtlt adjust dow (RandomStandardDate y m d) = let cd = mkcd d m y in dir 1 dow cd `gtlt` modify (adjust 8) day cd
    epochDay = mkcd 1 March 2000

constructorUnits :: TestTree
constructorUnits = testGroup "Constructor"
  [
     testCase "CalendarDate 30 February 2000 is not a valid date" $ calendarDate 30 February 2000 @?= Nothing
    ,testCase "CalendarDate 1 October 1582 is not a valid date" $ calendarDate 1 October 1582 @?= Nothing
    ,testCase "Gregorian.fromWeekDate 1 Sunday 2000 = 26.Dec.1999" $ G.fromWeekDate 1 Sunday 2000 @?= calendarDate 26 December 1999
    ,testCase "Gregorian.fromWeekDate 5 Sunday 2000 = 23.Jan.2000" $ G.fromWeekDate 5 Sunday 2000 @?= calendarDate 23 January 2000
    ,testCase "Iso.fromWeekDate 1 Sunday 2000 = 9.Jan.2000" $ Iso.fromWeekDate 1 Sunday 2000 @?= calendarDate 9 January 2000
    ,testCase "Iso.fromWeekDate 5 Sunday 2000 = 6.Feb.2000" $ Iso.fromWeekDate 5 Sunday 2000 @?= calendarDate 6 February 2000
    ,testCase "Holidays in year 2000" $ test2k (usaHolidays 2000)
    ,testCase "Holidays in year 2001" $ test2001 (usaHolidays 2001)
    ,testCase "Holidays in year 1582" $ test1582 (usaHolidays 1582)
  ]
    where
      test2k hs = do
        assertEqual "length == 8" 8 (length hs)
        assertEqual "First Monday Sept = 4.Sept.2000" (fromJust $ calendarDate 4 September 2000) (hs !! 3)
        assertEqual "Third Monday Jan = 17.Jan.2000" (fromJust $ calendarDate 17 January 2000) (hs !! 4)
        assertEqual "Second Tuesday Feb = 8.Feb.2000" (fromJust $ calendarDate 8 February 2000) (hs !! 5)
        assertEqual "Fourth Thursday Nov = 23.Nov.2000" (fromJust $ calendarDate 23 November 2000) (hs !! 6)
      test2001 hs = do
        assertEqual "length == 7" 7 (length hs)
        assertEqual "First Monday Sept = 3.Sept.2001" (fromJust $ calendarDate 3 September 2001) (hs !! 3)
        assertEqual "Third Monday Jan = 15.Jan.2001" (fromJust $ calendarDate 15 January 2001) (hs !! 4)
        assertEqual "Second Tuesday Feb = 13.Feb.2001" (fromJust $ calendarDate 13 February 2001) (hs !! 5)
        assertEqual "Fourth Thursday Nov = 22.Nov.2001" (fromJust $ calendarDate 22 November 2001) (hs !! 6)
      test1582 hs = do
        assertEqual "length == 2" 2 (length hs)
        assertEqual "Fourth Thursday Nov = 25.Nov.1582" (fromJust $ calendarDate 25 November 1582) (hs !! 1)
      usaHolidays y = catMaybes $ ($ y) <$>
        [
           calendarDate 1 January               -- New Year
          ,calendarDate 4 July                  -- Independence Day 
          ,calendarDate 25 December             -- Christmas
          ,fromNthDay First Monday September    -- Labor day
          ,fromNthDay Third Monday January      -- MLK day
          ,fromNthDay Second Tuesday February   -- Presidents day
          ,fromNthDay Fourth Thursday November  -- Thanksgiving
          ,calendarDate 29 February             -- Not a holiday but will sometimes be absent
        ]

lensUnits :: TestTree
lensUnits = testGroup "Lens"
  [
     testCase "31-January-2000 + 2M == 31-March-2000" $ modify (+2) monthl <$> janEnd @?= calendarDate 31 March 2000
    ,testCase "31-January-2000 + 1M == 29-February-2000" $ modify (+1) monthl <$> janEnd @?= calendarDate 29 February 2000
    ,testCase "15-November-1582 - 1M == 15-October-1582" $ modify (subtract 1) monthl <$> calendarDate 15 November 1582 @?= firstValidDate
    ,testCase "14-November-1582 - 1M == 15-October-1582 (clamped)" $ modify (subtract 1) monthl <$> calendarDate 14 November 1582 @?= firstValidDate
    ,testCase "29-February-2000 + 1Y == 28-February-2001" $ modify (+1) year <$> leapFeb @?= calendarDate 28 February 2001
    ,testCase "15-October-1583 - 1Y == 15-October-1582" $ modify (subtract 1) year <$> calendarDate 15 October 1583 @?= firstValidDate
    ,testCase "14-October-1583 - 1Y == 15-October-1582 (clamped)" $ modify (subtract 1) year <$> calendarDate 14 October 1583 @?= firstValidDate
    ,testCase "31-December-2000 + 1D == 1-January-2001" $ modify (+1) day <$> endYear @?= calendarDate 1 January 2001
    ,testCase "16-October-1583 - 1D == 15-October-1582" $ modify (subtract 1) day <$> calendarDate 16 October 1582 @?= firstValidDate
    ,testCase "15-October-1583 - 1D == 15-October-1582 (clamped)" $ modify (subtract 1) day <$> calendarDate 15 October 1582 @?= firstValidDate
  ]
    where
      janEnd = calendarDate 31 January 2000
      leapFeb = calendarDate 29 February 2000
      endYear = calendarDate 31 December 2000
      firstValidDate = calendarDate 15 October 1582
