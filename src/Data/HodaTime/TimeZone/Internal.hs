module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier(..)
  ,TransitionInfo(..)
  ,TransitionExpression(..)
  ,TransitionExpressionInfo(..)
  ,TransitionExpressionDetails(..)
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
import Data.HodaTime.Instant.Internal (Instant(..), add, minus)
import Data.HodaTime.Calendar.Gregorian.Internal (nthDayToDayOfMonth, yearMonthDayToDays, instantToYearMonthDay)
import Data.HodaTime.Duration.Internal (fromNanoseconds, fromSeconds)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IMap

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo { tiUtcOffset :: Int, tiIsDst :: Bool, tiAbbreviation :: String }
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

data TransitionExpressionDetails = TransitionExpressionDetails { firstExpressionInstant :: Instant, transitionExpression :: TransitionExpressionInfo }
  deriving (Eq, Show)

-- UTC instant to transition

type UtcTransitionsMap = Map Instant TransitionInfo

emptyUtcTransitions :: UtcTransitionsMap
emptyUtcTransitions = Map.empty

addUtcTransition :: Instant -> TransitionInfo -> UtcTransitionsMap -> UtcTransitionsMap
addUtcTransition = Map.insert

activeTransitionFor :: Instant -> TimeZone -> TransitionInfo
activeTransitionFor i (TimeZone _ utcM _ _ tds)
  | hasExplicitTransition i tds = snd . fromMaybe (Map.findMin utcM) $ Map.lookupLE i utcM     -- TODO: The findMin case should be impossible actually
  | otherwise                   = undefined

nextTransition :: Instant -> TimeZone -> (Instant, TransitionInfo)
nextTransition i (TimeZone _ utcM _ _ tds)
  | hasExplicitTransition i tds = fromMaybe (Map.findMax utcM) $ Map.lookupGT i utcM
  | otherwise                   = undefined

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

type CalDateTransitionsMap = IntervalMap (IntervalEntry Instant) TransitionInfo

emptyCalDateTransitions :: CalDateTransitionsMap
emptyCalDateTransitions = IMap.empty

addCalDateTransition :: IntervalEntry Instant -> IntervalEntry Instant -> TransitionInfo -> CalDateTransitionsMap -> CalDateTransitionsMap
addCalDateTransition b e = IMap.insert interval
  where
    interval = Interval b e

calDateTransitionsFor :: Instant -> TimeZone -> [TransitionInfo]
calDateTransitionsFor i (TimeZone _ _ cdtMap _ tds) = fromExpressionDetails i tds tInfos toExprTIs
  where
    search = fmap snd . IMap.search (Entry i)
    tInfos = search cdtMap
    toExprTIs = search . transExprInfoToMap i

-- TODO: decide what we should be doing with these errors
aroundCalDateTransition :: Instant -> TimeZone -> (TransitionInfo, TransitionInfo)
aroundCalDateTransition i (TimeZone _ _ cdtMap _ tds) = fromExpressionDetails i tds (before, after) toExprTIs
    where
      toExprTIs (TransitionExpressionInfo _ _ stdTI dstTI) = (stdTI, dstTI)   -- NOTE: Only possible answer since gap only happens switching to DST
      before = snd . go . flip IMap.search cdtMap . IMap.high . fromMaybe (error "around.before: fixme") . IMap.bounds $ front
      after = snd . fst . fromMaybe (error "around.after: fixme") . IMap.leastView $ back
      (front, back) = IMap.splitAfter (Entry i) cdtMap
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
      ,transitionExpressionDetails :: Maybe TransitionExpressionDetails
    }
  deriving (Eq, Show)

-- helper functions

-- TODO: replace uses of this function with fromExpressionDetails
hasExplicitTransition :: Instant -> Maybe TransitionExpressionDetails -> Bool
hasExplicitTransition i tds = fromExpressionDetails i tds True (const False)

fromExpressionDetails :: Instant -> Maybe TransitionExpressionDetails -> a -> (TransitionExpressionInfo -> a) -> a
fromExpressionDetails _ Nothing def _ = def
fromExpressionDetails i (Just (TransitionExpressionDetails fei tei)) def f
  | i < fei = def
  | otherwise = f tei

transExprInfoToMap :: Instant -> TransitionExpressionInfo -> CalDateTransitionsMap
transExprInfoToMap i (TransitionExpressionInfo startExpr endExpr stdTI dstTI) = mkMap entries mempty
  where
    mkMap [] m = m
    mkMap ((b, e, ti):xs) m = mkMap xs $ addCalDateTransition b e ti m
    entries = [(Smallest, Entry beforeStart, stdTI), (Entry start', Entry beforeEnd, dstTI), (Entry end', Largest, stdTI)]
    offsetGap = fromSeconds $ tiUtcOffset dstTI - tiUtcOffset stdTI
    start = expressionToInstant i startExpr
    start' = start `add` offsetGap
    beforeStart = flip minus (fromNanoseconds 1) start
    end = expressionToInstant i endExpr
    end' = end `minus` offsetGap
    beforeEnd = flip minus (fromNanoseconds 1) end

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
    go (JulianExpression cly d s) = error "need julian year day function"