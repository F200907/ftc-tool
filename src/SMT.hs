{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT (SMTInstance (..), checkValidity, cvc5, z3, contractCondition, Validity (..), initialState, withDebug) where

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
import SMTLIB.Backends.Process (Config (args, exe, std_err), StdStream (CreatePipe), new, toBackend)
import qualified SMTLIB.Backends.Process as SMTLIB
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

data Config = Config SMTLIB.Config Bool

withDebug :: SMT.Config -> Bool -> SMT.Config
withDebug (Config cfg _) = Config cfg

z3 :: SMT.Config
z3 = Config (SMTLIB.Config {std_err = CreatePipe, exe = "z3", args = ["-in"]}) False

cvc5 :: SMT.Config
cvc5 = Config (SMTLIB.Config {std_err = CreatePipe, exe = "cvc5", args = []}) False

text2Builder :: Text -> Builder
text2Builder = byteString . encodeUtf8

assert :: SMT.Config -> Solver -> SMTFormula -> IO ()
assert cfg solver formula =
  let cmd = smtOp ("assert" <+> smtify formula)
   in do
        command_ solver $ text2Builder cmd
        printDebug' cfg cmd

declareState :: SMT.Config -> Solver -> Int -> IO ()
declareState cfg solver idx =
  let cmd = smtOp ("declare-const" <+> indexedState idx <+> "State")
   in do
        command_ solver $ text2Builder cmd
        printDebug' cfg cmd

setupSolver :: SMT.Config -> Solver -> IO ()
setupSolver cfg solver = do
  command_ solver "(set-logic ALL)"
  command_ solver "(set-option :produce-unsat-cores true) ; enable generation of unsat cores"
  printDebug cfg "(set-logic ALL)\n(set-option :produce-unsat-cores true) ; enable generation of unsat cores"

checkValidity :: SMT.Config -> SMTInstance -> IO Validity
checkValidity cfg@(Config libCfg _) (SMTInstance {variables, conditions, problem}) = do
  process <- new libCfg
  solver <- initSolver NoQueuing (toBackend process)
  setupSolver cfg solver
  let info = unpack $ genState variables
  mapM_ (command_ solver . string8) (lines info)
  printDebug cfg info
  let states' = foldl (\acc c -> states c `Set.union` acc) (states problem) conditions
  mapM_ (declareState cfg solver) (Set.toList states')
  mapM_ (assert cfg solver) conditions
  let p = smtOp ("assert" <+> smtOp ("not" <+> smtify problem))
  printDebug' cfg p
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

initialState :: Validity -> Maybe (Map Text Int)
initialState Valid = Nothing
initialState (Counterexample states') = (return . snd . head) states'

printDebug :: SMT.Config -> String -> IO ()
printDebug (Config _ False) _ = return ()
printDebug (Config _ True) s = putStrLn s

printDebug' :: SMT.Config -> Text -> IO ()
printDebug' cfg t = printDebug cfg (unpack t)

-- testInstance = SMTInstance {variables = ["x"], conditions = [Predicate (StatePredicate 1 (Exp.Not (Exp.LessThan (AVar "x") (Constant 0))))], problem = mc' simpleProg "m"}
