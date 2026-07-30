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
import Data.HodaTime.Instant.Internal (Instant(..), minus, bigBang)
import Data.HodaTime.Offset.Internal (Offset(..), adjustInstant)
import Data.HodaTime.Duration.Internal (fromNanoseconds)
import Data.HodaTime.Calendar.Gregorian.Internal (nthDayToDayOfMonth, yearMonthDayToDays, maxDaysInMonth, instantToYearMonthDay)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IMap
import Data.Hashable (Hashable(..))

data TZIdentifier = UTC | Zone String
  deriving (Eq, Show)

instance Hashable TZIdentifier where
  hashWithSalt s UTC      = hashWithSalt s (0 :: Int)
  hashWithSalt s (Zone n) = s `hashWithSalt` (1 :: Int) `hashWithSalt` n

data TransitionInfo = TransitionInfo { tiUtcOffset :: Offset, tiIsDst :: Bool, tiAbbreviation :: String }
  deriving (Eq, Show)

instance Hashable TransitionInfo where
  hashWithSalt s (TransitionInfo off isDst abbr) = s `hashWithSalt` off `hashWithSalt` isDst `hashWithSalt` abbr

data TransitionExpression =
    NthDayExpression Int Int Int Int  -- ^ month, nthDay, day, seconds
  | JulianExpression Bool Int Int     -- ^ countLeaps, day, seconds
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
    f (dstStart, dstEnd, stdTI, dstTI) = if i <= dstStart || i >= dstEnd then stdTI else dstTI

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

-- NOTE: Only ever called (from 'resolve') when a local time falls in a spring-forward gap.  This looks partial but is
-- total by construction: every zone's 'CalDateTransitionsMap' tiles [Smallest, Largest] (see the constructors), so the
-- 'IMap.splitAfter' below always yields a non-empty 'front' and 'back' — the 'IMap.bounds'\/'leastView' Nothing cases
-- cannot occur.  A top-level empty search ('go []') only happens in the fixed (historical) region: the expression
-- region always returns a single interval and is handled by 'go [expr]', so the bracketing transitions are always
-- fixed ('bomb' is unreachable).
aroundCalDateTransition :: Instant -> TimeZone -> (TransitionInfo, TransitionInfo)
aroundCalDateTransition i (TimeZone _ _ cdtMap) = go . fmap snd . IMap.search (Entry i) $ cdtMap
    where
      go [] = (before, after)
      go [(TransitionInfoExpression (TransitionExpressionInfo _ _ stdTI dstTI))] = (stdTI, dstTI) -- NOTE: Should be the only way this happens
      go x = error $ "aroundCalDateTransition: unreachable - a gap search should return [] or a single expression, got: " ++ show x
      before = fromTransInfo i bomb id . snd . go' . flip IMap.search cdtMap . IMap.high . fromMaybe (error "aroundCalDateTransition: unreachable - empty 'front' (the map always tiles from Smallest)") . IMap.bounds $ front
      after = fromTransInfo i bomb id . snd . fst . fromMaybe (error "aroundCalDateTransition: unreachable - empty 'back' (the map always tiles to Largest)") . IMap.leastView $ back
      (front, back) = IMap.splitAfter (Entry i) cdtMap
      go' [] = error "aroundCalDateTransition: unreachable - no interval before the gap (the map always tiles from Smallest)"
      go' [tei] = tei
      go' _ = error "aroundCalDateTransition: unreachable - more than one interval at the boundary before the gap"
      bomb = error "aroundCalDateTransition: unreachable - bracketing transition was an expression, not fixed ('go []' only fires in the fixed region)"

-- | Represents a time zone.  A 'TimeZone' can be used to instanciate a 'ZoneDateTime' from either and 'Instant' or a 'CalendarDateTime'
data TimeZone =
  TimeZone
    {
       zoneName :: TZIdentifier
      ,utcTransitionsMap :: UtcTransitionsMap
      ,calDateTransitionsMap :: CalDateTransitionsMap
    }

-- | Shows a 'TimeZone' by its identity only (the transition maps are a derived cache, not part of identity).
instance Show TimeZone where
  show tz = case zoneName tz of
    UTC    -> "<TimeZone UTC>"
    Zone n -> "<TimeZone " ++ show n ++ ">"

-- | Two 'TimeZone's are equal when they denote the same zone (compared by identifier).  The transition maps are a
--   derived lookup cache fully determined by the identifier, so they are not part of the zone's identity.
instance Eq TimeZone where
  a == b = zoneName a == zoneName b

instance Hashable TimeZone where
  hashWithSalt s = hashWithSalt s . zoneName

-- NOTE: 'TimeZone' deliberately has no 'NFData' instance.  'calDateTransitionsMap' is an 'IntervalMap' from the
-- 'fingertree' package, which depends only on 'base' and therefore provides no 'NFData' instance to force it.  A
-- partial 'rnf' that forced only the identifier would silently leave the bulk of the value (the transition maps)
-- unevaluated, which would be a misleading 'NFData', so we omit it entirely.  The same reasoning applies to
-- 'ZonedDateTime' and 'OffsetDateTime', which embed a 'TimeZone'.

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

-- NOTE: We have to store expressions in the year they take effect so this is the first place we can resolve
--       the actual map.  Otherwise we'd have to create two per year
buildFixedTransIMap :: (Instant, Instant, TransitionInfo, TransitionInfo) -> IntervalMap (IntervalEntry Instant) TransitionInfo
buildFixedTransIMap (start, end, stdTI, dstTI) = mkMap entries mempty
  where
    mkMap [] m = m
    mkMap ((b, e, ti):xs) m = mkMap xs $ addEntry b e ti m
    addEntry b e ti = IMap.insert (Interval b e) ti
    entries = [(Smallest, Entry beforeStart, stdTI), (Entry start', Entry beforeEnd, dstTI), (Entry end', Largest, stdTI)]
    (start', beforeStart) = adjust start dstTI stdTI
    (end', beforeEnd) = adjust end stdTI dstTI
    adjust tran ti prevTI = (x, beforeX)
      where
        x = adjustInstant (tiUtcOffset ti) tran
        beforeX = flip minus (fromNanoseconds 1) . adjustInstant (tiUtcOffset prevTI) $ tran

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
    go (JulianExpression countLeaps day s) = Instant days' (fromIntegral s) 0
      where
        -- POSIX day-of-year transition.  The @n@ form (countLeaps == True) is 0-based and counts 29.Feb; the @Jn@
        -- form (countLeaps == False) is 1-based and never counts 29.Feb, so 1.Mar is always day 60.  NOTE: this is a
        -- day-of-year, unrelated to the Julian *calendar*.
        offset
          | countLeaps          = day
          | isLeap && day >= 60  = day
          | otherwise            = day - 1
        isLeap = maxDaysInMonth (toEnum 1) y == 29
        days' = fromIntegral $ yearMonthDayToDays y (toEnum 0) 1 + offset