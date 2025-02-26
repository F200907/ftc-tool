{-# LANGUAGE OverloadedStrings #-}

module Data.Trace.TraceLogicParser (parseTraceFormula) where

import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.ExpressionParser (parseArithmeticExpr, parseBooleanExpr)
import Data.Text (Text)
import Data.Trace.TraceLogic
import Text.Megaparsec
import Util.ParserUtil

type TParser = Parser TraceFormula

chopToken :: Text
chopToken = "~"

muToken :: Text
muToken = "\\"

muApplToken :: Text
muApplToken = "."

conjunctionToken :: Text
conjunctionToken = "&&"

disjunctionToken :: Text
disjunctionToken = "||"

idRelToken :: Text
idRelToken = "Id"

assignRelToken :: Text
assignRelToken = "Sb_"

assignRelDelim :: Text
assignRelDelim = "^"

recVarToken :: Text
recVarToken = "X_"

prePostRelDelim :: Text
prePostRelDelim = ","

pStateFormula :: TParser
pStateFormula = StateFormula <$> parseBooleanExpr

pRecursiveVariable :: TParser
pRecursiveVariable = do
  _ <- symbol recVarToken
  RecursiveVariable <$> identifier

pBinaryRelation :: TParser
pBinaryRelation = do
  BinaryRelation
    <$> choice
      [ try $
          brackets
            ( do
                p <- lexeme parseBooleanExpr
                _ <- symbol prePostRelDelim
                PrePostConditions p <$> lexeme parseBooleanExpr
            ),
        try
          ( do
              _ <- symbol assignRelToken
              x <- lexeme $ braces identifier <|> parens identifier <|> identifier
              _ <- symbol assignRelDelim
              a <- lexeme $ braces parseArithmeticExpr <|> parens parseArithmeticExpr
              return $ Sb x a
          ),
        do
          _ <- symbol idRelToken
          return Id
      ]

pMu :: TParser
pMu = do
  _ <- symbol muToken
  _ <- symbol recVarToken
  x <- lexeme identifier
  _ <- symbol muApplToken
  Mu x <$> lexeme pTrace

traceOperatorTable :: [[Operator Parser TraceFormula]]
traceOperatorTable =
  [ [binary chopToken Chop],
    [binary conjunctionToken Conjunction, binary disjunctionToken Disjunction]
  ]

pTTerm :: TParser
pTTerm =
  choice
    [ parens pTrace,
      try pMu,
      try pRecursiveVariable,
      try pBinaryRelation,
      try pStateFormula
    ]

pTrace :: TParser
pTrace = makeExprParser pTTerm traceOperatorTable

parseTraceFormula :: TParser
parseTraceFormula = pTrace