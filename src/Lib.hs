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
    ftc,
    parseProblem,
    parseModel,
    initialState,
    withDebug,
    normalise,
    depth,
    reinforce,
    constraints,
    ftcCondition,
    Variables (..)
  )
where

import Data.Expression
import Data.ExpressionParser
import Data.FTC.FiniteTraceCalculus
import Data.Trace.Program
import Data.Trace.ProgramParser
import Data.Trace.TraceLogic
import Data.Trace.TraceLogicParser
import SMT.Solver
import SMT.ModelParser
import Data.ProblemParser
import Data.Trace.Normalise
import Data.Variable
