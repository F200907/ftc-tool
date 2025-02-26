module Main (Main.main) where

import Lib
import Options.Applicative
import Data.Char (toLower)

data Mode = Parse | STF deriving (Show)

data Args = Args
  { input :: Maybe String,
    output :: Maybe String,
    mode :: Mode,
    pretty :: Bool
  }
  deriving (Show)

argInput :: Parser (Maybe String)
argInput =
  optional $
    strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file (if none then STDIN is used)"
      )

argOutput :: Parser (Maybe String)
argOutput =
  optional $
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILENAME"
          <> help "Output file (if none then STDOUT is used)"
      )

parseMode :: ReadM Mode
parseMode = eitherReader $ \s -> case map toLower s of
    "parse" -> Right Parse
    "stf" -> Right STF
    _ -> Left $ "could not parse the correct mode from \'" ++ s ++ "\'"

argMode :: Parser Mode
argMode = option parseMode (long "mode"
    <> short 'm'
    <> showDefault
    <> value Parse
    <> metavar "PARSE|STF"
    <> help "Mode")

argHumanRedable :: Parser Bool
argHumanRedable = switch (long "pretty"
    <> short 'p'
    <> showDefault
    <> help "Pretty printing")

args :: Parser Args
args = Args <$> argInput <*> argOutput <*> argMode <*> argHumanRedable

main :: IO ()
main = entry =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc ""
            <> header ""
        )

entry :: Args -> IO ()
entry = print