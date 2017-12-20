module Data.HodaTime.TimeZone.ParseTZ
(
  parsePosixString
)
where

import Data.HodaTime.TimeZone.Internal
import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))

type ExpParser = Parsec String ()

parsePosixString :: String -> TransExpressionOrInfo
parsePosixString tzStr = case runParser p_posixTzString () tzStr tzStr of
        Left err -> error $ show err
        Right r -> r

p_posixTzString :: ExpParser TransExpressionOrInfo
p_posixTzString = do
  stdID <- p_tzIdentifier <?> "Standard TZ identifier"
  stdOffset <- p_offset <?> "TZ Offset"
  p_dstExpression stdID stdOffset <|> (return . TInfo . TransitionInfo stdOffset False $ stdID)

p_dstExpression :: String -> Int -> ExpParser TransExpressionOrInfo
p_dstExpression stdID stdOffset = do
  dstID <- p_tzIdentifier
  dstOffset <- option (stdOffset + toHour 1) p_offset
  stdExpr <- char ',' *> p_transitionExpression
  dstExpr <- char ',' *> p_transitionExpression
  let stdTI = TransitionInfo stdOffset False stdID
  let dstTI = TransitionInfo dstOffset True dstID
  return . TExp $ TransitionExpressionInfo stdExpr dstExpr stdTI dstTI

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

p_offset :: ExpParser Int
p_offset = do
  sign <- (negate <$ char '-') <|> (option id (id <$ char '+'))
  hours <- toHour . read <$> many1 digit
  minutes <- option 0 $ (* 60) . read <$> optionalDigit
  seconds <- option 0 $ read <$> optionalDigit
  return . sign $ hours + minutes + seconds
    where
      optionalDigit = char ':' *> many1 digit

p_transitionExpression :: ExpParser TransitionExpression
p_transitionExpression = do
  te <- p_julianExpression <|> p_nthDayExpression
  time <- option (toHour 2) (char '/' *> p_offset)
  return . te $ time

p_julianExpression :: ExpParser (Int -> TransitionExpression)
p_julianExpression = do
  countLeapDay <- option True (False <$ char 'J')
  JulianExpression countLeapDay . read <$> many1 digit

p_nthDayExpression :: ExpParser (Int -> TransitionExpression)
p_nthDayExpression = do
  m <- char 'M' *> p_number
  nth <- char '.' *> p_number
  d <- char '.' *> p_number
  return $ NthDayExpression m nth d
    where
      p_number = read <$> many1 digit

-- helper functions

toHour :: Int -> Int
toHour x = x * 60 * 60