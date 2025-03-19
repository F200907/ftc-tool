{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Trace.ProgramParser (parseStatement, parseMethodDefinition, parseProgram) where

import Data.ExpressionParser (parseArithmeticExpr, parseBooleanExpr)
import Data.Text (Text)
import Data.Trace.Program (MethodDefinition, Program (Program, main, methods), Statement (Assignment, Condition, Method, Sequence, Skip))
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.ParserUtil
import Data.FTC.Contract (Contract)
import Data.Expression (BooleanExpr(BTrue))

type SParser = Parser Statement

skipToken :: Text
skipToken = "skip"

assignToken :: Text
assignToken = ":="

ifToken :: Text
ifToken = "if"

thenToken :: Text
thenToken = "then"

elseToken :: Text
elseToken = "else"

pSkip :: SParser
pSkip = do
  _ <- lexeme (string skipToken)
  return Skip

pMethod :: SParser
pMethod = do
  methodName <- lexeme identifier
  _ <- symbol "()" <?> "invocation"
  return (Method methodName)

pAssignment :: SParser
pAssignment = do
  variable <- lexeme identifier
  _ <- symbol assignToken
  expr <- lexeme parseArithmeticExpr
  return $ Assignment variable expr

pStatement :: SParser
pStatement =
  choice
    [ try pMethod,
      try pCondition,
      try pSkip,
      try pAssignment,
      parens pSequence
    ]

pCondition :: SParser
pCondition = do
  _ <- symbol ifToken
  b <- lexeme parseBooleanExpr
  _ <- symbol thenToken
  s1 <- pSequence
  _ <- symbol elseToken
  Condition b s1 <$> lexeme pSequence

pSequence :: SParser
pSequence = do
  try
    ( do
        s <- lexeme pStatement
        _ <- symbol ";"
        Sequence s <$> lexeme pSequence
    )
    <|> lexeme pStatement

parseStatement :: SParser
parseStatement = pSequence

parseContract :: Parser Contract
parseContract = choice [try (do
  pre <- brackets parseBooleanExpr
  post <- brackets parseBooleanExpr
  return (pre, post)), return (BTrue, BTrue)]

parseMethodDefinition :: Parser MethodDefinition
parseMethodDefinition = do
  contract <- parseContract
  methodName <- identifier <?> "method name"
  methodBody <- braces parseStatement
  return (methodName, contract, methodBody)

parseProgram :: Parser Program
parseProgram = programParser' (Program {methods = [], main = Nothing})
  where
    programParser' program@Program {methods, main} =
      choice
        [ try
            ( do
                methodDef <- lexeme parseMethodDefinition
                programParser' (Program {methods = methods ++ [methodDef], main})
            ),
          try
            ( do
                main' <- lexeme parseStatement
                programParser' (Program {methods, main = Just main'})
            ),
          do
            _ <- eof
            return program
        ]

_testParse :: IO ()
_testParse = parseTest parseProgram "[x >= 0][(#x = 2)=>(y=1)]even{if x = 0 then y := 1 else x := x - 1; odd()}\n odd{if x = 0 then \n y := 0 \n\t else \n \t x := x - 1; even()} /* this is a test */ x:=3;even()"

_simpleProg :: IO ()
_simpleProg = parseTest parseProgram "[x >= 0][x = 0] m{if x > 0 then (x := x - 1; m()) else skip} m()"