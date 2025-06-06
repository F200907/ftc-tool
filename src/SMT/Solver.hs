{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.Solver (SMTInstance (..), checkValidity, cvc5, z3, contractCondition, Validity (..), initialState, withDebug, ftcCondition, checkValidityFTC) where

import Data.ByteString (toStrict)
import Data.ByteString.Builder (Builder, byteString, string8)
import qualified Data.Expression as Exp
import Data.FTC.FiniteTraceCalculus (FTCProblem (..), SideCondition, constraints, fromFormula, ftc, mc)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Trace.Program (Program (main, methods), contract, contracts, lookupMethod)
import Data.Trace.TraceLogic (TraceFormula (..))
import qualified Data.Variable as Variable
import Debug.Trace (trace)
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import SMT.Formula (SMTFormula (..), instantiateConstantPred)
import SMT.Instance
import SMT.ModelParser (parseModel)
import SMT.SMTUtil (SMTify (smtify, states), genState, indexedState, smtOp, (<+>))
import SMTLIB.Backends (QueuingFlag (NoQueuing), Solver, command, command_, initSolver)
import SMTLIB.Backends.Process (Config (args, exe, std_err), StdStream (CreatePipe), new, toBackend)
import qualified SMTLIB.Backends.Process as SMTLIB
import Text.Megaparsec (parse)
import Util.PrettyUtil (mapsTo)

data Validity = Valid | Counterexample [(Int, Map Text Int)] deriving (Show, Eq)

instance (Pretty Validity) where
  pretty :: Validity -> Doc ann
  pretty Valid = "Valid"
  pretty (Counterexample sequence') = "Counterexample:" <> line <> align' states'
    where
      align' x = vsep $ reverse $ zipWith (P.<+>) (reverse x) ("" : repeat mapsTo)
      states' = map (\(_, m) -> pretty m) sequence'

data Config' = Config SMTLIB.Config Bool

withDebug :: Config' -> Bool -> Config'
withDebug (Config cfg _) = Config cfg

z3 :: Config'
z3 = Config (SMTLIB.Config {std_err = CreatePipe, exe = "z3", args = ["-in"]}) False

cvc5 :: Config'
cvc5 = Config (SMTLIB.Config {std_err = CreatePipe, exe = "cvc5", args = []}) False

text2Builder :: Text -> Builder
text2Builder = byteString . encodeUtf8

assert :: Config' -> Solver -> SMTFormula a -> IO ()
assert cfg solver formula =
  let cmd = smtOp ("assert" <+> smtify formula)
   in do
        printDebug' cfg cmd
        command_ solver $ text2Builder cmd

declareState :: Config' -> Solver -> Int -> IO ()
declareState cfg solver idx =
  let cmd = smtOp ("declare-const" <+> indexedState idx <+> "State")
   in do
        printDebug' cfg cmd
        command_ solver $ text2Builder cmd

setupSolver :: Config' -> Solver -> IO ()
setupSolver cfg solver = do
  printDebug cfg "(reset)\n(set-logic ALL)\n(set-option :produce-unsat-cores true) ; enable generation of unsat cores"
  command_ solver "(reset)"
  command_ solver "(set-logic ALL)"
  command_ solver "(set-option :produce-unsat-cores true) ; enable generation of unsat cores"

checkValidity :: Config' -> SMTInstance a -> IO Validity
checkValidity cfg@(Config libCfg _) inst@(SMTInstance {conditions, problem}) = do
  let vars = Set.toList (Variable.variables inst)
  process <- new libCfg
  solver <- initSolver NoQueuing (toBackend process)
  setupSolver cfg solver
  let info = unpack $ genState vars
  printDebug cfg info
  mapM_ (command_ solver . string8) (lines info)
  let states' = foldl (\acc c -> states c `Set.union` acc) (states problem) conditions
  mapM_ (declareState cfg solver) (Set.toList $ states' `Set.difference` Set.singleton 0)
  mapM_ (assert cfg solver) conditions
  let p = smtOp ("assert" <+> smtOp ("not" <+> smtify problem))
  printDebug' cfg p
  command_ solver $ text2Builder p
  s <- command solver "(check-sat)"
  let invalid = decodeUtf8 (toStrict s)
  printDebug' cfg invalid
  case invalid of
    "sat" ->
      ( do
          model <- decodeUtf8 . toStrict <$> command solver "(get-model)"
          return $ Counterexample (counterexample vars model)
      )
    "success" -> return $ Counterexample [(-1, Map.fromList [("empty state", 0)])]
    "unsat" -> return Valid
    _ -> do
      printDebug' cfg "something went wrong with the smt-solver"
      return $ Counterexample []

contractCondition :: Program -> Text -> SMTInstance a
contractCondition p m = case lookupMethod m (methods p) of
  Just s -> case contract m (methods p) of
    Just (pre, post) ->
      let problem = mc (contracts p) 1 s post
       in instanceOf [StatePredicate 1 pre] problem
    _ -> invalidInstance
  _ -> invalidInstance

ftcCondition :: Program -> TraceFormula -> FTCProblem SideCondition
ftcCondition = ftc

checkValidityFTC :: (Eq a, Show a) => Config' -> FTCProblem a -> IO Validity
checkValidityFTC cfg (FTCProblem {dependencies, inst}) = case dependencies of
  [] -> checkValidity cfg inst
  ((a, p) : deps) -> do
    validity <- checkValidityFTC cfg p
    let val = (\case Valid -> True; Counterexample _ -> False) validity
    let (conds, prob) = (conditions inst, problem inst)
    let sub x = instantiateConstantPred x a val
    checkValidityFTC cfg (FTCProblem deps (SMTInstance (map sub conds) (sub prob)))

counterexample :: [Text] -> Text -> [(Int, Map Text Int)]
counterexample vars raw = case parse (parseModel vars) "SMT-solver" raw of
  Left err -> error (show err ++ "\n" ++ show raw)
  Right model -> Map.toAscList model

initialState :: Validity -> Maybe (Map Text Int)
initialState Valid = Nothing
initialState (Counterexample states') = (return . snd . head) states'

printDebug :: Config' -> String -> IO ()
printDebug (Config _ False) _ = return ()
printDebug (Config _ True) s = putStrLn s

printDebug' :: Config' -> Text -> IO ()
printDebug' cfg t = printDebug cfg (unpack t)
