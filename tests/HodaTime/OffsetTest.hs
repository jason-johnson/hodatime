module HodaTime.OffsetTest
(
  offsetTests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.HodaTime.Offset

offsetTests :: TestTree
offsetTests = testGroup "Offset Tests" [qcProps, unitTests]

-- top level tests

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [mathProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

-- properties

mathProps :: TestTree
mathProps = testGroup "Math"
  [
     QC.testProperty "fromSeconds x `add` fromSeconds y == fromSeconds (x+y)" $ test fromSeconds add plus
    ,QC.testProperty "fromSeconds x `minus` fromSeconds y == fromSeconds (x-y)" $ test fromSeconds minus subtr
    ,QC.testProperty "fromSeconds x `add` fromMinutes y == fromMinutes (x+y)" $ test fromMinutes add plus
    ,QC.testProperty "fromSeconds x `minus` fromMinutes y == fromMinutes (x-y)" $ test fromMinutes minus subtr
    ,QC.testProperty "fromSeconds x `add` fromHours y == fromHours (x+y)" $ test fromHours add plus
    ,QC.testProperty "fromSeconds x `minus` fromHours y == fromHours (x-y)" $ test fromHours minus subtr
  ]
  where
    test f g h x y = f x `g` f y == f (h x y)

-- helper functions

-- NOTE: The following 2 functions only exist to lock in the type so the QC tests can work
plus :: Int -> Int -> Int
plus = (+)

subtr :: Int -> Int -> Int
subtr = (-)
