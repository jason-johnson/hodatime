module Parser
(
   DisplayInfo(..)
  ,Settings(..)
  ,parse
)
where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readMaybe)

data DisplayInfo = HeaderOnly | TransitionsOnly | AllData
  deriving Show

data Settings = Settings
  {
     setVersion :: Int
    ,setDisplay :: DisplayInfo
    ,setFile :: String
  }

parseVersion :: ReadM Int
parseVersion = eitherReader (go . readMaybe)
  where
    go (Just 1) = Right 1
    go (Just 2) = Right 2
    go (Just 3) = Right 3
    go _ = Left "version must be between 1-3"


optVersion :: Parser Int
optVersion = option parseVersion (short 'v' <> help "Version to display" <> showDefault <> value 1 <> metavar "INT")

optHeader :: Parser DisplayInfo
optHeader = flag AllData HeaderOnly (short 'h' <> help "Print only the header" )

optTransOnly :: Parser DisplayInfo
optTransOnly = flag' TransitionsOnly (short 't' <> help "Print only the transitions" )

optDisplay :: Parser DisplayInfo
optDisplay = (optHeader <|> optTransOnly)

settings :: Parser Settings
settings = Settings <$> optVersion <*> optDisplay <*> argument str (metavar "FILE")

parse :: IO Settings
parse = execParser opts
  where
     opts = info (settings <**> helper) (fullDesc <> progDesc "Print the contents of an olson file in text form" <> header "olson-print - print olson content" )