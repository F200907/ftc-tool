{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Trace.ProgramParser (parseStatement, parseMethodDefinition, parseProgram) where

-- import Error.Diagnose (TabSize (TabSize), WithUnicode (WithUnicode), addFile, defaultStyle, printDiagnostic, stderr, Diagnostic, Note)
-- import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle, hints, HasHints)

import Data.ExpressionParser (parseArithmeticExpr, parseBooleanExpr)
import Data.Text (Text)
import Data.Trace.Program (MethodDefinition, Program (Program, main, methods), Statement (Assignment, Condition, Method, Sequence, Skip))
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.ParserUtil

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

parseMethodDefinition :: Parser MethodDefinition
parseMethodDefinition = do
  methodName <- identifier <?> "method name"
  methodBody <- braces parseStatement
  return (methodName, methodBody)

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
_testParse = parseTest parseProgram "even{if x = 0 then y := 1 else x := x - 1; odd()}\n odd{if x = 0 then \n y := 0 \n\t else \n \t x := x - 1; even()} /* this is a test */ x:=3;even()"

-- parseProgram :: String -> Text -> Either (Diagnostic Text) Statement
-- parseProgram filename content =
--   let res = runParser parser filename content
--    in case res of
--         Left bundle ->
--           let diag = errorDiagnosticFromBundle Nothing "Parse error on program" Nothing bundle
--               diag' = addFile diag filename (unpack content)
--            in Left diag' -- error $ printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag'
--                 -- return Skip
--         Right res' -> Right res'

-- parseProgramIO :: String -> Text -> IO ()
-- parseProgramIO filename content =
--   let res = parseProgram filename content in
--     case res of
--       Left diag -> printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag
--       Right statement -> print $ pretty statement

-- instance HasHints Void Text where
--   hints _ = []