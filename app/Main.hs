{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (Main.main) where

import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (pack, unpack)
import Lib hiding (reinforce)
import qualified Lib
import Options.Applicative
import qualified Prettyprinter as P
import Text.Megaparsec (parse)

data Mode = Verify | Parse | STF deriving (Show)

data Args = Args
  { input :: Maybe String,
    output :: Maybe String,
    mode :: Mode,
    pretty :: Bool,
    debug :: Bool,
    reinforce :: Bool
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
argMode =
  option
    parseMode
    ( long "mode"
        <> short 'm'
        <> showDefault
        <> value Verify
        <> metavar "VERIFY|PARSE|STF"
        <> help "Mode"
    )

argHumanRedable :: Parser Bool
argHumanRedable =
  switch
    ( long "pretty"
        <> short 'p'
        <> showDefault
        <> help "Pretty printing"
    )

argDebug :: Parser Bool
argDebug =
  switch
    ( long "debug"
        <> short 'd'
        <> showDefault
        <> help "Print debugging"
    )

argReinforce :: Parser Bool
argReinforce =
  switch
    ( long "reinforce"
        <> short 'r'
        <> showDefault
        <> help "Reinforce the program's contracts"
    )

args :: Parser Args
args = Args <$> argInput <*> argOutput <*> argMode <*> argHumanRedable <*> argDebug <*> argReinforce

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
  let res = parse parseProblem "" s
  case res of
    Left err -> print err
    Right (tf, p') -> do
      when
        (isJust tf)
        ( do
            putStrLn' "Parsed trace formula:"
            putStrLn' tf
        )

      let p = if reinforce a then Lib.reinforce p' else p'
      putStrLn' "Parsed program:"
      putStrLn' p
      putStrLn' ""
      mapM_
        ( \(m, _, _) ->
            let cond = contractCondition p m
                smt = withDebug z3 (debug a)
             in do
                  putStrLn' ("Checking for the contract of " ++ unpack m ++ ":")
                  valid <- checkValidity smt cond
                  putStrLn' valid
                  case initialState valid of
                    Nothing -> return ()
                    Just state ->
                      let initial = foldl (\acc v -> if Map.member v acc then acc else Map.insert v 0 acc) state (variables p)
                          final = bigStep' p initial (methodBody p m)
                       in do
                            putStrLn' "SOS semantics yields:"
                            putStr' initial
                            putStrLn' " -> "
                            putStrLn' final
                  putStrLn ""
        )
        (methods p)
      ( case tf of
          Just tf' ->
            mapM_
              ( \inst ->
                  let smt = withDebug z3 (debug a)
                   in do
                    putStrLn' "Checking SMT problem:"
                    putStrLn' inst
                    valid <- checkValidity smt inst
                    putStrLn' valid
              )
              (ftcCondition p tf')
          Nothing -> return ()
        )
      return ()
  return ()
  where
    show' x = if pretty a then (show . P.pretty) x else toString x
    putStrLn' x = putStrLn (show' x)
    putStr' x = putStr (show' x)

class ToString a where
  toString :: a -> String

instance {-# OVERLAPPABLE #-} ToString String where
  toString :: String -> String
  toString = id

instance {-# OVERLAPPABLE #-} (Show a) => ToString a where
  toString :: a -> String
  toString = show
