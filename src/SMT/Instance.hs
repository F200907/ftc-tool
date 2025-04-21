{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SMT.Instance (SMTInstance (..), invalidInstance, instanceOf, constantPredicates) where

import Data.Set (Set)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (..), line, vsep)
import SMT.Formula (SMTFormula (..))
import qualified Data.Set as Set
import Data.Variable

data SMTInstance a = SMTInstance {conditions :: [SMTFormula a], problem :: SMTFormula a} deriving (Show, Eq)

instance Pretty a => Pretty (SMTInstance a) where
  pretty :: SMTInstance a-> Doc ann
  pretty (SMTInstance {conditions, problem}) =
    (if null conditions then "" else "Conditions:" <> line <> vsep (map pretty conditions) <> line)
      <> "Problem:"
      <> line
      <> pretty problem

invalidInstance :: SMTInstance a
invalidInstance = SMTInstance {conditions = [], problem = Bot}

instance Variables (SMTInstance a) where
  variables :: SMTInstance a-> Set Text
  variables (SMTInstance {conditions, problem}) = foldl (\acc smt -> acc `Set.union` variables smt) (variables problem) conditions

constantPredicates :: SMTInstance a -> [a]
constantPredicates (SMTInstance {conditions, problem}) = concatMap constantPredicates' (problem : conditions)
  where
    constantPredicates' Top = []
    constantPredicates' Bot = []
    constantPredicates' (Implies a b) = concatMap constantPredicates' [a, b]
    constantPredicates' (Not a) = constantPredicates' a
    constantPredicates' (And a b) = concatMap constantPredicates' [a, b]
    constantPredicates' (Or a b) = concatMap constantPredicates' [a, b]
    constantPredicates' (StatePredicate _ _) = []
    constantPredicates' (BinaryPredicate {}) = []
    constantPredicates' (IdentityPredicate _) = []
    constantPredicates' (AssignmentPredicate {}) = []
    constantPredicates' (ConstantPred t) = [t]

instanceOf :: [SMTFormula a] -> SMTFormula a -> SMTInstance a
instanceOf conditions problem = SMTInstance {conditions = conditions, problem = problem}