{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
    parseBooleanExpr,
    parseArithmeticExpr,
    evaluate,
    substitute,
    BooleanExpr (..),
    ArithmeticExpr (..),
    Valuation,
    Program (..),
    parseProgram,
    smallStep,
    smallStep',
    bigStep,
    bigStep',
    parseStatement,
    parseMethodDefinition,
    emptyProgram,
    TraceFormula (..),
    strongestTraceFormula,
    strongestTraceFormula',
    parseTraceFormula,
    rename,
  )
where

import Data.Expression
import Data.ExpressionParser
import Data.Trace.Program
import Data.Trace.ProgramParser
import Data.Trace.TraceLogic
import Data.Trace.TraceLogicParser

someFunc :: IO ()
someFunc = print (1 :: Integer)

-- someFunc :: IO ()
-- someFunc = print $ pretty $ runParser parser "if 1 then skip else skip"
