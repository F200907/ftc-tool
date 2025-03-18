{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT (SMTInstance (..), checkValidity, cvc5, z3) where

import Control.Monad (foldM_, unless)
import Data.ByteString (toStrict)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, string8)
import Data.Expression (ArithmeticExpr (AVar, Plus, Constant), BooleanExpr (Equal))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import SMT.SMTFormula (SMTFormula (Bot, Predicate, Top))
import SMT.SMTPredicate (SMTPredicate (StatePredicate))
import SMT.SMTUtil (SMTify (smtify), StateType (StateType), genState, smtOp, (<+>), indexedState)
import SMTLIB.Backends (QueuingFlag (NoQueuing), Solver, command, command_, initSolver)
import SMTLIB.Backends.Process (Config (Config, args, exe, std_err), StdStream (CreatePipe), new, toBackend)
import Data.FTC.FiniteTraceCalculus (mc')
import Data.Trace.Program (simpleProg)
import SMT.SMTFormula (SMTFormula(..))
import qualified Data.Expression as Exp


-- import Data.ByteString.Char8 (toString)

data SMTInstance = SMTInstance {variables :: [Text], conditions :: [SMTFormula], problem :: SMTFormula}

z3 :: Config
z3 = Config {std_err = CreatePipe, exe = "z3", args = ["-in"]}

cvc5 :: Config
cvc5 = Config {std_err = CreatePipe, exe = "cvc5", args = []}

text2Builder :: Text -> Builder
text2Builder = byteString . encodeUtf8

assert :: Solver -> SMTFormula -> IO ()
assert solver formula = command_ solver $ text2Builder $ smtOp ("assert" <+> smtify formula)

declareState :: Solver -> Int -> IO ()
declareState solver idx = command_ solver $ text2Builder $ smtOp ("declare-const" <+> indexedState idx <+> "State")

setupSolver :: Solver -> IO ()
setupSolver solver = do
  command_ solver "(set-logic ALL)"
  command_ solver "(set-option :produce-unsat-cores true) ; enable generation of unsat cores"

checkValidity :: Config -> SMTInstance -> IO Bool
checkValidity cfg (SMTInstance {variables, conditions, problem}) = do
  process <- new cfg
  solver <- initSolver NoQueuing (toBackend process)
  setupSolver solver
  let info = unpack $ genState (StateType variables)
  mapM_ (command_ solver . string8) (lines info)
  mapM_ (declareState solver) [1..5]
  mapM_ (assert solver) conditions
  let p = smtOp ("assert" <+> smtOp ("not" <+> smtify problem))
  command_ solver $ text2Builder p
  s <- command solver "(check-sat)"
  let valid = decodeUtf8 (toStrict s) == "unsat"
  unless valid (do
    model <- command solver "(get-model)"
    print model)
  return valid
--   return $ sat == "unsat"

testInstance = SMTInstance {variables = ["x"], conditions = [Predicate (StatePredicate 1 (Exp.Not (Exp.LessThan (AVar "x") (Constant 0))))], problem = mc' simpleProg "m"}

