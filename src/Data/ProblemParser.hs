{-# LANGUAGE OverloadedStrings #-}

module Data.ProblemParser (parseProblem) where

import Data.Text ()
import Data.Trace.Program (Program)
import Data.Trace.ProgramParser (parseProgram)
import Data.Trace.TraceLogic (TraceFormula)
import Data.Trace.TraceLogicParser (parseTraceFormula)
import Text.Megaparsec
import Util.ParserUtil

type PParser = Parser (Maybe TraceFormula, Program)

parseProblem :: PParser
parseProblem = do
  tf <- optional (try (lexeme $ between "\"" "\"" parseTraceFormula))
  p <- lexeme parseProgram
  return (tf, p)