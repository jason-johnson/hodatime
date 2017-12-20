module Data.HodaTime.TimeZone.ParseTZ
(
  parsePosixString
)
where

import Data.HodaTime.TimeZone.Internal
import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))

type ExpParser = Parsec String ()

parsePosixString :: String -> TransitionExpressionInfo
parsePosixString tzStr = case runParser p () tzStr tzStr of
        Left err -> error $ show err
        Right r -> r
    where
        p = p_posixTzString  -- TODO: if this is all there is, replace p with this above

p_posixTzString :: ExpParser TransitionExpressionInfo
p_posixTzString = do
  stdID <- p_tzIdentifier <?> "Standard TZ identifier"
  stdOffset <- p_offset <?> "TZ Offset"
  dstID <- p_tzIdentifier <?> "DST TZ identifier"                     -- NOTE: This would actually be optional but we don't handle that
  dstOffset <- option (stdOffset + toHour 1) p_offset
  let stdTI = TransitionInfo stdOffset False stdID
  let dstTI = TransitionInfo dstOffset True dstID
  let stdExpr = TransitionExpression 0 0 0 0
  let dstExpr = TransitionExpression 0 0 0 0
  return $ TransitionExpressionInfo stdExpr dstExpr stdTI dstTI

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
  sign <- (char '-' *> return negate) <|> (option id (id <$ char '+'))
  hours <- toHour . read <$> many1 digit
  minutes <- option 0 $ (* 60) . read <$> optionalDigit
  seconds <- option 0 $ read <$> optionalDigit
  return . sign $ hours + minutes + seconds
    where
      optionalDigit = char ':' *> many1 digit

-- helper functions

toHour :: Int -> Int
toHour x = x * 60 * 60