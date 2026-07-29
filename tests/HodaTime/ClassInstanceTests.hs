module HodaTime.ClassInstanceTests
(
  classInstanceTests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromJust)
import Control.DeepSeq (deepseq)
import Data.Hashable (hash)

import Data.HodaTime.Instant (Instant, fromSecondsSinceUnixEpoch)
import qualified Data.HodaTime.Duration as Dur
import Data.HodaTime.LocalTime (localTime, LocalTime)
import qualified Data.HodaTime.Offset as Off
import Data.HodaTime.Interval (interval)
import Data.HodaTime.CalendarDate (CalendarDate)
import Data.HodaTime.CalendarDateTime (at)
import Data.HodaTime.Calendar.Gregorian (Month(..))
import qualified Data.HodaTime.Calendar.Gregorian as G

classInstanceTests :: TestTree
classInstanceTests = testGroup "NFData / Hashable instances" [hashDiscriminates, hashConsistent, nfdataForces]

-- | Confirms every field of a value participates in its hash (i.e. the 'Hashable' instances actually fold each
--   field rather than collapsing to a constant).  Distinct values are compared and expected to hash differently.
hashDiscriminates :: TestTree
hashDiscriminates = testGroup "hash distinguishes distinct values"
  [
     testCase "Instant: day field"      $ assertBool "" (hash i0 /= hash iDay)
    ,testCase "Instant: second field"   $ assertBool "" (hash i0 /= hash iSec)
    ,testCase "Duration: seconds"       $ assertBool "" (hash (Dur.fromSeconds (1 :: Int)) /= hash (Dur.fromSeconds (2 :: Int)))
    ,testCase "Duration: nanoseconds"   $ assertBool "" (hash (Dur.fromNanoseconds (1 :: Int)) /= hash (Dur.fromNanoseconds (2 :: Int)))
    ,testCase "Offset"                  $ assertBool "" (hash (Off.fromSeconds (3600 :: Int)) /= hash (Off.fromSeconds (7200 :: Int)))
    ,testCase "LocalTime"               $ assertBool "" (hash lt1 /= hash lt2)
    ,testCase "Interval"                $ assertBool "" (hash (interval i0 iDay) /= hash (interval i0 iSec))
    ,testCase "CalendarDate"            $ assertBool "" (hash cd1 /= hash cd2)
    ,testCase "CalendarDateTime"        $ assertBool "" (hash (at cd1 lt1) /= hash (at cd2 lt1))
  ]

-- | Confirms 'hash' agrees with '(==)': equal values reached by different construction paths must hash equally.
hashConsistent :: TestTree
hashConsistent = testGroup "hash agrees with (==)"
  [
     testCase "Duration: seconds vs nanoseconds" $ do
       let a = Dur.fromSeconds (1 :: Int)
           b = Dur.fromNanoseconds (1000000000 :: Int)
       assertEqual "equal values"  a b
       assertEqual "equal hashes"  (hash a) (hash b)
  ]

-- | Confirms the 'NFData' instances force a value to normal form without error.
nfdataForces :: TestTree
nfdataForces = testGroup "NFData forces values"
  [
     testCase "Instant"          $ assertBool "" (i0 `deepseq` True)
    ,testCase "Duration"         $ assertBool "" (Dur.fromSeconds (1 :: Int) `deepseq` True)
    ,testCase "Offset"           $ assertBool "" (Off.fromSeconds (3600 :: Int) `deepseq` True)
    ,testCase "LocalTime"        $ assertBool "" (lt1 `deepseq` True)
    ,testCase "Interval"         $ assertBool "" (interval i0 iDay `deepseq` True)
    ,testCase "CalendarDate"     $ assertBool "" (cd1 `deepseq` True)
    ,testCase "CalendarDateTime" $ assertBool "" (at cd1 lt1 `deepseq` True)
  ]

-- shared values

i0, iDay, iSec :: Instant
i0   = fromSecondsSinceUnixEpoch 0
iDay = fromSecondsSinceUnixEpoch 86400
iSec = fromSecondsSinceUnixEpoch 5

lt1, lt2 :: LocalTime
lt1 = fromJust $ localTime 10 30 0 0
lt2 = fromJust $ localTime 11 30 0 0

cd1, cd2 :: CalendarDate G.Gregorian
cd1 = fromJust $ G.calendarDate 10 March 2020
cd2 = fromJust $ G.calendarDate 11 March 2020
