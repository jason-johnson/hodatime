module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier(..)
  ,TransitionInfo(..)
  ,TransitionExpression(..)
  ,TransitionExpressionInfo(..)
  ,TransExpressionOrInfo(..)
  ,UtcTransitionsMap
  ,LeapsMap
  ,IntervalEntry(..)
  ,CalDateTransitionsMap
  ,TimeZone(..)
  ,emptyUtcTransitions
  ,addUtcTransition
  ,activeTransitionFor
  ,nextTransition
  ,emptyLeapsMap
  ,importLeaps
  ,addLeapTransition
  ,mergeLeapMaps
  ,activeLeapsFor
  ,emptyCalDateTransitions
  ,addCalDateTransition
  ,calDateTransitionsFor
  ,aroundCalDateTransition
  ,expressionToInstant
  ,yearExpressionToInstant
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant(..))
import Data.HodaTime.Calendar.Gregorian.Internal (nthDayToDayOfMonth, yearMonthDayToDays, instantToYearMonthDay)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow ((>>>), second)
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IMap

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo { tiUtcOffset :: Int, isDst :: Bool, tiAbbreviation :: String }
  deriving (Eq, Show)

data TransitionExpression =
  NthDayExpression
  {
     teMonth :: Int
    ,teNthDay :: Int
    ,teDay :: Int
    ,teSeconds :: Int
  }
  | JulianExpression { jeCountLeaps :: Bool, jeDay :: Int, jeSeconds :: Int }
  deriving (Eq, Show)

data TransitionExpressionInfo = TransitionExpressionInfo
  {
     startExpression :: TransitionExpression
    ,endExpression :: TransitionExpression
    ,stdTransInfo :: TransitionInfo
    ,dstTransInfo :: TransitionInfo
  }
  deriving (Eq, Show)

data TransExpressionOrInfo = TInfo TransitionInfo | TExp TransitionExpressionInfo
  deriving (Eq, Show)

-- UTC instant to transition

type UtcTransitionsMap = Map Instant TransExpressionOrInfo

emptyUtcTransitions :: UtcTransitionsMap
emptyUtcTransitions = Map.empty

addUtcTransition :: Instant -> TransExpressionOrInfo -> UtcTransitionsMap -> UtcTransitionsMap
addUtcTransition = Map.insert

activeTransitionFor :: Instant -> UtcTransitionsMap -> TransitionInfo
activeTransitionFor i utcM = resolveTI i . snd . fromMaybe (Map.findMin utcM) $ Map.lookupLE i utcM     -- TODO: The findMin case should be impossible actually

nextTransition :: Instant -> UtcTransitionsMap -> (Instant, TransitionInfo)
nextTransition i ts = fromMaybe (Map.findMax ts) >>> second (resolveTI i) $ Map.lookupGT i ts

-- Leap seconds

type LeapsMap = Map Instant Int

emptyLeapsMap :: LeapsMap
emptyLeapsMap = Map.empty

importLeaps :: [(Instant, Int)] -> LeapsMap
importLeaps = Map.fromList

addLeapTransition :: Instant -> Int -> LeapsMap -> LeapsMap
addLeapTransition = Map.insert

mergeLeapMaps :: LeapsMap -> LeapsMap -> LeapsMap
mergeLeapMaps = Map.union

activeLeapsFor :: Instant -> LeapsMap -> Int
activeLeapsFor i leapsM = fromMaybe 0 $ fmap snd $ Map.lookupLE i leapsM

-- CalendarDate to transition

data IntervalEntry a =
    Smallest
  | Entry a
  | Largest
  deriving (Eq, Ord, Show)

type CalDateTransitionsMap = IntervalMap (IntervalEntry Instant) TransExpressionOrInfo

emptyCalDateTransitions :: CalDateTransitionsMap
emptyCalDateTransitions = IMap.empty

addCalDateTransition :: IntervalEntry Instant -> IntervalEntry Instant -> TransExpressionOrInfo -> CalDateTransitionsMap -> CalDateTransitionsMap
addCalDateTransition b e = IMap.insert interval
  where
    interval = Interval b e

calDateTransitionsFor :: Instant -> CalDateTransitionsMap -> [TransitionInfo]
calDateTransitionsFor i = fmap (resolveTI i . snd) . IMap.search (Entry i)

-- TODO: decide what we should be doing with these errors
aroundCalDateTransition :: Instant -> CalDateTransitionsMap -> (TransitionInfo, TransitionInfo)
aroundCalDateTransition i ts = (before, after)
  where
    before = resolveTI i . snd . go . flip IMap.search ts . IMap.high . fromMaybe (error "around.before: fixme") . IMap.bounds $ front
    after = resolveTI i . snd . fst . fromMaybe (error "around.after: fixme") . IMap.leastView $ back
    (front, back) = IMap.splitAfter (Entry i) ts
    go [] = error "aroundCalDateTransition: no before transitions"
    go [tei] = tei
    go _ = error "aroundCalDateTransition: too many before transitions"

-- | Represents a time zone.  A 'TimeZone' can be used to instanciate a 'ZoneDateTime' from either and 'Instant' or a 'CalendarDateTime'
data TimeZone =
  TimeZone
    {
       zoneName :: TZIdentifier
      ,utcTransitionsMap :: UtcTransitionsMap
      ,calDateTransitionsMap :: CalDateTransitionsMap
      ,leapsMap :: LeapsMap
    }
  deriving (Eq, Show)

-- helper functions

resolveTI :: Instant -> TransExpressionOrInfo -> TransitionInfo
resolveTI _  (TInfo ti) = ti
resolveTI instant (TExp (TransitionExpressionInfo startExpr endExpr stdTI dstTI)) = ti
  where
    start = expressionToInstant instant startExpr
    ti = if instant < start then stdTI else ti'
    dst = expressionToInstant instant endExpr
    ti' = if instant < dst then dstTI else stdTI

expressionToInstant :: Instant -> TransitionExpression -> Instant
expressionToInstant instant = yearExpressionToInstant y
  where
    y = let (yr, _, _) = instantToYearMonthDay instant in fromIntegral yr

yearExpressionToInstant :: Int -> TransitionExpression -> Instant
yearExpressionToInstant y = go
  where
    go (NthDayExpression m nth day s) = Instant days' (fromIntegral s) 0
      where
        m' = toEnum m
        d = nthDayToDayOfMonth nth day m' y
        days' = fromIntegral $ yearMonthDayToDays y m' d
    go (JulianExpression cl d s) = undefined