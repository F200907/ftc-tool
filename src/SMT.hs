{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT (SMTInstance (..), checkValidity, cvc5, z3, contractCondition, Validity (..)) where

import Control.Monad (unless)
import Data.ByteString (toStrict)
import Data.ByteString.Builder (Builder, byteString, string8)
import qualified Data.Expression as Exp
import Data.FTC.FiniteTraceCalculus (mc)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Trace.Program (Program (methods), contract, contracts, lookupMethod)
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import SMT.ModelParser (parseModel)
import SMT.SMTFormula (SMTFormula (..))
import SMT.SMTPredicate (SMTPredicate (StatePredicate))
import SMT.SMTUtil (SMTify (smtify, states), genState, indexedState, smtOp, (<+>))
import SMTLIB.Backends (QueuingFlag (NoQueuing), Solver, command, command_, initSolver)
import SMTLIB.Backends.Process (Config (Config, args, exe, std_err), StdStream (CreatePipe), new, toBackend)
import Text.Megaparsec (parse)
import Util.PrettyUtil (mapsTo)

-- import Data.ByteString.Char8 (toString)

data SMTInstance = SMTInstance {variables :: [Text], conditions :: [SMTFormula], problem :: SMTFormula} deriving (Show)

data Validity = Valid | Counterexample [(Int, Map Text Int)] deriving (Show)

instance (Pretty Validity) where
  pretty :: Validity -> Doc ann
  pretty Valid = "Valid"
  pretty (Counterexample sequence') = "Counterexample:" <> line <> align' states'
    where
      align' x = vsep $ reverse $ zipWith (P.<+>) (reverse x) ("" : repeat mapsTo)
      states' = map (\(_, m) -> pretty m) sequence'

invalidInstance :: SMTInstance
invalidInstance = SMTInstance {variables = [], conditions = [], problem = Bot}

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

checkValidity :: Config -> SMTInstance -> IO Validity
checkValidity cfg (SMTInstance {variables, conditions, problem}) = do
  process <- new cfg
  solver <- initSolver NoQueuing (toBackend process)
  setupSolver solver
  let info = unpack $ genState variables
  mapM_ (command_ solver . string8) (lines info)
  let states' = foldl (\acc c -> states c `Set.union` acc) (states problem) conditions
  mapM_ (declareState solver) (Set.toList states')
  mapM_ (assert solver) conditions
  let p = smtOp ("assert" <+> smtOp ("not" <+> smtify problem))
  command_ solver $ text2Builder p
  s <- command solver "(check-sat)"
  let valid = decodeUtf8 (toStrict s) == "unsat"
  ( if valid
      then return Valid
      else
        ( do
            model <- decodeUtf8 . toStrict <$> command solver "(get-model)"
            return $ Counterexample (counterexample variables model)
        )
    )

contractCondition :: Program -> Text -> SMTInstance
contractCondition p m = case lookupMethod m (methods p) of
  Just s -> case contract m (methods p) of
    Just (pre, post) ->
      let problem = mc (contracts p) 1 s post
       in SMTInstance {variables = Set.toList (Exp.variables problem), conditions = [Predicate (StatePredicate 1 pre)], problem = problem}
    _ -> invalidInstance
  _ -> invalidInstance

counterexample :: [Text] -> Text -> [(Int, Map Text Int)]
counterexample vars raw = case parse (parseModel vars) "SMT-solver" raw of
  Left err -> error (show err)
  Right model -> Map.toAscList model

-- testInstance = SMTInstance {variables = ["x"], conditions = [Predicate (StatePredicate 1 (Exp.Not (Exp.LessThan (AVar "x") (Constant 0))))], problem = mc' simpleProg "m"}
