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
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

-- instance MonadThrow (QCM.PropertyM IO)

patternTests :: TestTree
patternTests = testGroup "Pattern Tests" [scProps, qcProps, unitTests]

-- top level tests

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" []

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [ calDateTimeProps, calDateProps, localTimeProps ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

-- properties

calDateTimeProps :: TestTree
calDateTimeProps = testGroup "CalendarDateTime conversion"
  [
     QC.testProperty "format ps CalendarDateTime -> parse ps CalendarDateTime == id" $ testCdtFormatToParseIdentity ps True
    ,QC.testProperty "format pf CalendarDateTime -> parse pf CalendarDateTime == id" $ testCdtFormatToParseIdentity pf False
    ,QC.testProperty "format pF CalendarDateTime -> parse pF CalendarDateTime == id" $ testCdtFormatToParseIdentity pF True
    ,QC.testProperty "format pg CalendarDateTime -> parse pg CalendarDateTime == id" $ testCdtFormatToParseIdentity pg False
    ,QC.testProperty "format pG CalendarDateTime -> parse pG CalendarDateTime == id" $ testCdtFormatToParseIdentity pG True
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
  ]
  where
    seconds True s = s
    seconds False _ = 0
    testLtFormatToParseIdentity pat useSeconds (RandomTime h m s) = monadicIO $ do
      let lt = fromMaybe (error "impossible") $ localTime h m (seconds useSeconds s) 0
      lt' <- run $ parse pat $ format pat lt
      QCM.assert $ lt == lt'