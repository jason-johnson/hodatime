module Data.HodaTime.TimeZone.ParseTZ
(
  parsePosixString
)
where

import Data.HodaTime.TimeZone.Internal
import Data.HodaTime.Offset.Internal (Offset(..))
import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))

type ExpParser = Parsec String ()

parsePosixString :: String -> (Either TransitionInfo TransitionExpressionInfo)
parsePosixString tzStr = case runParser p_posixTzString () tzStr tzStr of
        Left err -> error $ show err
        Right r -> r

p_posixTzString :: ExpParser (Either TransitionInfo TransitionExpressionInfo)
p_posixTzString = do
  stdID <- p_tzIdentifier <?> "Standard TZ identifier"
  stdOffset <- p_offset <?> "TZ Offset"
  p_dstExpression stdID stdOffset <|> (return . Left . TransitionInfo stdOffset False $ stdID)

-- NOTE: Transition times have to be UTC for the returned expressions
p_dstExpression :: String -> Offset -> ExpParser (Either TransitionInfo TransitionExpressionInfo)
p_dstExpression stdID stdOffset@(Offset stdOffsetSecs) = do
  dstID <- p_tzIdentifier
  dstOffset <- option (Offset $ stdOffsetSecs + toHour 1) p_offset
  startExpr <- char ',' *> p_transitionExpression stdOffsetSecs
  endExpr <- char ',' *> p_transitionExpression (toOffsetSecs dstOffset)
  let stdTI = TransitionInfo stdOffset False stdID
  let dstTI = TransitionInfo dstOffset True dstID
  return . Right $ TransitionExpressionInfo startExpr endExpr stdTI dstTI
  where
    toOffsetSecs (Offset secs) = secs

p_tzIdentifier :: ExpParser String
p_tzIdentifier = p_tzSpecialIdentifier <|> p_tzNormalIdentifier

p_tzSpecialIdentifier :: ExpParser String
p_tzSpecialIdentifier = do
  idt <- char '<' *> many1 (noneOf ">")
  ("<" ++ idt ++ ">") <$ char '>'

p_tzNormalIdentifier :: ExpParser String
p_tzNormalIdentifier = do
  start <- count 3 letter
  rest <- many . noneOf $ ":,+-" ++ ['0'..'9']
  return $ start ++ rest

p_offset :: ExpParser Offset
p_offset = Offset . negate <$> p_time

p_time :: ExpParser Int
p_time = do
  sign <- (negate <$ char '-') <|> (option id (id <$ char '+'))
  hours <- toHour . read <$> many1 digit
  minutes <- option 0 $ (* 60) . read <$> optionalDigit
  seconds <- option 0 $ read <$> optionalDigit
  return . sign $ hours + minutes + seconds
    where
      optionalDigit = char ':' *> many1 digit

-- NOTE: Remove offset seconds to get to UTC time
p_transitionExpression :: Int -> ExpParser TransitionExpression
p_transitionExpression offsetSecs = te <*> time'
  where
    te = p_julianExpression <|> p_nthDayExpression
    time = option (toHour 2) (char '/' *> p_time)
    time' = (\x -> x - offsetSecs) <$> time

p_julianExpression :: ExpParser (Int -> TransitionExpression)
p_julianExpression = JulianExpression <$> cntLp <*> d
  where
    cntLp = option True (False <$ char 'J')
    d = read <$> many1 digit

p_nthDayExpression :: ExpParser (Int -> TransitionExpression)
p_nthDayExpression = NthDayExpression <$> m <*> nth <*> d
  where
    m =   char 'M' *> (subtract 1 <$> p_number)
    nth = char '.' *> (adjust <$> p_number)
    d =   char '.' *> p_number
    p_number = read <$> many1 digit
    adjust 5 = -1
    adjust n = n - 1

-- helper functions

toHour :: Int -> Int
toHour x = x * 60 * 60