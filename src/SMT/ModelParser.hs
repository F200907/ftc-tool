{-# LANGUAGE OverloadedStrings #-}

module SMT.ModelParser (parseModel) where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (eof, try), choice)
import Util.ParserUtil

type State = Map Text Int

type Model = Map Int State

type MParser = Parser Model

mkState :: Text
mkState = "mk-state"

defineFun :: Text
defineFun = "define-fun"

stateSort :: Text
stateSort = "State"

stateSymbol :: Text
stateSymbol = "state"

parseValue :: Parser Int
parseValue = choice [ try (
  do
    v <- parens (lexeme $ symbol "-" >> integer)
    return (-v)),
    integer
  ]

parseState :: [Text] -> Parser (Int, State)
parseState vars = do
  _ <- symbol defineFun
  _ <- symbol stateSymbol
  idx <- integer
  _ <- symbol "()"
  _ <- lexeme $ symbol stateSort
  state <-
    lexeme $
      parens
        ( do
            _ <- symbol mkState
            foldM
              ( \acc x -> do
                  v <- parseValue
                  return $ Map.insert x v acc
              )
              Map.empty
              vars
        )
  return (idx, state)

parseModel :: [Text] -> MParser
parseModel vars = lexeme $ symbol "(" >> parseModel' Map.empty
  where
    parseModel' :: Model -> MParser
    parseModel' model =
      lexeme $
        choice
          [ try
              ( do
                  (idx, state) <- lexeme $ parens $ parseState vars
                  parseModel' (Map.insert idx state model)
              ),
            do
              _ <- symbol ")" >> eof
              return model
          ]