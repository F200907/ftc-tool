module Lib
  ( parseBooleanExpr,
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
    methodBody,
    bigStep',
    parseStatement,
    parseMethodDefinition,
    emptyProgram,
    TraceFormula (..),
    strongestTraceFormula,
    strongestTraceFormula',
    parseTraceFormula,
    rename,
    z3,
    SMTInstance,
    checkValidity,
    contractCondition,
    cvc5,
    parseModel,
    initialState,
    withDebug,
    reinforce,
    Variables (..)
  )
where

import Data.Expression
import Data.ExpressionParser
import Data.Trace.Program
import Data.Trace.ProgramParser
import Data.Trace.TraceLogic
import Data.Trace.TraceLogicParser
import SMT
import SMT.ModelParser
