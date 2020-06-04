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
import Control.Monad.Catch (MonadThrow)
import Data.Semigroup ((<>))
import Test.Tasty.HUnit
import HodaTime.Util
import Data.HodaTime.LocalTime (localTime)
import Data.HodaTime.CalendarDateTime (at, CalendarDateTime)
import qualified Data.HodaTime.Calendar.Gregorian as G
import Data.HodaTime.Pattern
import Data.HodaTime.Pattern.CalendarDate
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
qcProps = testGroup "(checked by QuickCheck)" [ calDateProps ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

-- properties

calDateProps :: TestTree
calDateProps = testGroup "CalendarDateTime conversion"
  [
     QC.testProperty "format pattern CalendarDateTime -> parse pattern CalendarDateTime == id" $ testFormatToParseIdentity
  ]
  where
    testFormatToParseIdentity (RandomStandardDate y mon d) (RandomTime h m s) = monadicIO $ do
      let cdt = maybe (error "impossible") id $ at <$> G.calendarDate d mon y <*> localTime h m s 0
      let pat = pat_year 4 <% pat_char '/' <> pat_month <% pat_char '/' <> pat_day <% pat_char ' ' <> pat_hour <% pat_char ':' <> pat_minute <% pat_char ':' <> pat_second
      cdt' <- run $ parse pat $ format pat cdt
      QCM.assert $ cdt == cdt'