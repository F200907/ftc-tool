-- {-# LANGUAGE OverloadedStrings #-}
module Main (Main.main) where

import Lib
import Options.Applicative
import Data.Char (toLower)
import Text.Megaparsec (parse)
import Data.Text (pack, unpack)
import qualified Prettyprinter as P
import Control.Monad (foldM_)

data Mode = Verify | Parse | STF deriving (Show)

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
    "verify" -> Right Verify
    "parse" -> Right Parse
    "stf" -> Right STF
    _ -> Left $ "could not parse the correct mode from \'" ++ s ++ "\'"

argMode :: Parser Mode
argMode = option parseMode (long "mode"
    <> short 'm'
    <> showDefault
    <> value Verify
    <> metavar "VERIFY|PARSE|STF"
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
entry a = case mode a of
  Verify -> verify a
  _ -> print a

source :: Args -> IO String
source a = maybe getContents readFile (input a)


verify :: Args -> IO ()
verify a = do
  s <- pack <$> source a
  let res = parse parseProgram "" s
  case res of
    Left err -> print err
    Right p -> do
      putStrLn' "Parsed the program successfully:"
      putStrLn' p
      putStrLn' ""
      mapM_ (\(m, _, _) -> let cond = contractCondition p m in do
        putStrLn' ("Checking for the contract of " ++ unpack m ++ ":")
        valid <- checkValidity z3 cond
        putStrLn' valid) (methods p)
  return ()
  where
    show' x = if pretty a then (show . P.pretty) x else show x
    putStrLn' x = putStrLn (show' x)