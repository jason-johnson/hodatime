module Data.HodaTime.TimeZone.Internal
(
   TZIdentifier(..)
  ,TransitionInfo(..)
  ,TransitionExpression(..)
  ,TransitionExpressionInfo(..)
  ,UtcTransitionsMap
  ,IntervalEntry(..)
  ,CalDateTransitionsMap
  ,TimeZone(..)
  ,emptyUtcTransitions
  ,addUtcTransition
  ,addUtcTransitionExpression
  ,activeTransitionFor
  ,nextTransition
  ,emptyCalDateTransitions
  ,addCalDateTransition
  ,addCalDateTransitionExpression
  ,calDateTransitionsFor
  ,aroundCalDateTransition
  ,fixedOffsetZone
  ,expressionToInstant
  ,yearExpressionToInstant
)
where

import Data.Maybe (fromMaybe)
import Data.HodaTime.Instant.Internal (Instant(..), add, minus, bigBang)
import Data.HodaTime.Offset.Internal (Offset(..), adjustInstant)
import Data.HodaTime.Duration.Internal (fromNanoseconds, fromSeconds)
import Data.HodaTime.Calendar.Gregorian.Internal (nthDayToDayOfMonth, yearMonthDayToDays, instantToYearMonthDay)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IMap

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

data TransitionInfo = TransitionInfo { tiUtcOffset :: Offset, tiIsDst :: Bool, tiAbbreviation :: String }
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

data TransitionInfoOrExp = 
    TransitionInfoFixed TransitionInfo
  | TransitionInfoExpression TransitionExpressionInfo
    deriving (Eq, Show)

-- UTC instant to transition

type UtcTransitionsMap = Map Instant TransitionInfoOrExp

emptyUtcTransitions :: UtcTransitionsMap
emptyUtcTransitions = Map.empty

addUtcTransition :: Instant -> TransitionInfo -> UtcTransitionsMap -> UtcTransitionsMap
addUtcTransition i fti = Map.insert i (TransitionInfoFixed fti)

addUtcTransitionExpression :: Instant -> TransitionExpressionInfo -> UtcTransitionsMap -> UtcTransitionsMap
addUtcTransitionExpression i texp = Map.insert i (TransitionInfoExpression texp)

activeTransitionFor :: Instant -> TimeZone -> TransitionInfo
activeTransitionFor i (TimeZone _ utcM _) = fromTransInfo i f id . snd . fromMaybe (Map.findMin utcM) $ Map.lookupLE i utcM     -- NOTE: The findMin case should be impossible
  where
    f (dstStart, dstEnd, stdTI, dstTI) = if i <= dstStart' || i > dstEnd' then stdTI else dstTI
      where
        dstStart' = adjustInstant (tiUtcOffset stdTI) dstStart
        dstEnd' = adjustInstant (tiUtcOffset dstTI) dstEnd

-- TODO: We would need to get the next year to complete this function but let's see if it's actually used before doing more work
nextTransition :: Instant -> TimeZone -> (Instant, TransitionInfo)
nextTransition i (TimeZone _ utcM _) = f . fromMaybe (Map.findMax utcM) $ Map.lookupGT i utcM
  where
    f (i', ti) = fromTransInfo i g (\ti' -> (i', ti')) ti
    g (dstStart, dstEnd, stdTI, dstTI) = if i < dstStart' then (dstStart', dstTI) else if i < dstEnd' then (dstEnd', stdTI) else error "nextTransition: need next year"
      where
        dstStart' = adjustInstant (tiUtcOffset stdTI) dstStart
        dstEnd' = adjustInstant (tiUtcOffset dstTI) dstEnd

-- CalendarDate to transition

data IntervalEntry a =
    Smallest
  | Entry a
  | Largest
  deriving (Eq, Ord, Show)

type CalDateTransitionsMap = IntervalMap (IntervalEntry Instant) TransitionInfoOrExp

emptyCalDateTransitions :: CalDateTransitionsMap
emptyCalDateTransitions = IMap.empty

addCalDateTransition :: IntervalEntry Instant -> IntervalEntry Instant -> TransitionInfo -> CalDateTransitionsMap -> CalDateTransitionsMap
addCalDateTransition b e fti = IMap.insert interval (TransitionInfoFixed fti)
  where
    interval = Interval b e

addCalDateTransitionExpression :: IntervalEntry Instant -> IntervalEntry Instant -> TransitionExpressionInfo -> CalDateTransitionsMap -> CalDateTransitionsMap
addCalDateTransitionExpression b e texp = IMap.insert interval (TransitionInfoExpression texp)
  where
    interval = Interval b e

calDateTransitionsFor :: Instant -> TimeZone -> [TransitionInfo]
calDateTransitionsFor i (TimeZone _ _ cdtMap) = concatMap (fromTransInfo i f (:[]) . snd) . search $ cdtMap
  where
    search = IMap.search (Entry i)
    f = fmap snd . search . buildFixedTransIMap

-- TODO: this function need major cleanup, this implementation is really nasty and almost certainly unsafe
aroundCalDateTransition :: Instant -> TimeZone -> (TransitionInfo, TransitionInfo)
aroundCalDateTransition i (TimeZone _ _ cdtMap) = go . fmap snd . IMap.search (Entry i) $ cdtMap
    where
      go [] = (before, after)
      go [(TransitionInfoExpression (TransitionExpressionInfo _ _ stdTI dstTI))] = (stdTI, dstTI) -- NOTE: Should be the only way this happens
      go x = error $ "aroundCalDateTransition: unexpected search result" ++ show x
      before = fromTransInfo i bomb id . snd . go' . flip IMap.search cdtMap . IMap.high . fromMaybe (error "around.before: fixme") . IMap.bounds $ front
      after = fromTransInfo i bomb id . snd . fst . fromMaybe (error "around.after: fixme") . IMap.leastView $ back
      (front, back) = IMap.splitAfter (Entry i) cdtMap
      go' [] = error "aroundCalDateTransition: no before transitions"
      go' [tei] = tei
      go' _ = error "aroundCalDateTransition: too many before transitions"
      bomb = error "aroundCalDateTransition: got expression when fixed expected"

-- | Represents a time zone.  A 'TimeZone' can be used to instanciate a 'ZoneDateTime' from either and 'Instant' or a 'CalendarDateTime'
data TimeZone =
  TimeZone
    {
       zoneName :: TZIdentifier
      ,utcTransitionsMap :: UtcTransitionsMap
      ,calDateTransitionsMap :: CalDateTransitionsMap
    }
  deriving (Eq, Show)

-- constructors

fixedOffsetZone :: String -> Offset -> (UtcTransitionsMap, CalDateTransitionsMap, TransitionInfo)
fixedOffsetZone tzName offset = (utcM, calDateM, tInfo)
    where
      utcM = addUtcTransition bigBang tInfo emptyUtcTransitions
      calDateM = addCalDateTransition Smallest Largest tInfo emptyCalDateTransitions
      tInfo = TransitionInfo offset False tzName

-- helper functions

fromTransInfo :: Instant -> ((Instant, Instant, TransitionInfo, TransitionInfo) -> a) -> (TransitionInfo -> a) -> TransitionInfoOrExp -> a
fromTransInfo _ _ f (TransitionInfoFixed ti) = f ti
fromTransInfo i f _ (TransitionInfoExpression (TransitionExpressionInfo startExpr endExpr stdTI dstTI)) = f (dstStart, dstEnd, stdTI, dstTI)
  where
    dstStart = expressionToInstant i startExpr
    dstEnd = expressionToInstant i endExpr

buildFixedTransIMap :: (Instant, Instant, TransitionInfo, TransitionInfo) -> IntervalMap (IntervalEntry Instant) TransitionInfo
buildFixedTransIMap (start, end, stdTI, dstTI) = mkMap entries mempty
  where
    mkMap [] m = m
    mkMap ((b, e, ti):xs) m = mkMap xs $ addEntry b e ti m
    addEntry b e ti = IMap.insert (Interval b e) ti
    toOffsetDuration (Offset lsecs) (Offset rsecs) = fromSeconds $ lsecs - rsecs
    entries = [(Smallest, Entry beforeStart, stdTI), (Entry start', Entry beforeEnd, dstTI), (Entry end', Largest, stdTI)]
    offsetGap = toOffsetDuration (tiUtcOffset dstTI) (tiUtcOffset stdTI)
    start' = start `add` offsetGap
    beforeStart = flip minus (fromNanoseconds 1) start
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
    go (JulianExpression _cly _d _s) = error "need julian year day function"