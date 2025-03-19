{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.SMTPredicate (SMTPredicate (..), predicate) where

import Data.Expression (ArithmeticExpr, BooleanExpr, VariableName, Variables (variables))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Trace.TraceLogic (BinaryRelation (Id, Sb), TraceFormula (BinaryRelation, Chop, Conjunction, Disjunction, Mu, RecursiveVariable, StateFormula), unfold)
import SMT.SMTUtil (SMTify (..), Stateful (stateful), indexedState, smtOp, (<+>))

data SMTPredicate
  = StatePredicate Int BooleanExpr
  | IdentityPredicate Int
  | AssignmentPredicate Int VariableName ArithmeticExpr
  | ConjunctionPredicate SMTPredicate SMTPredicate
  | DisjunctionPredicate SMTPredicate SMTPredicate
  | BotPredicate
  deriving (Show)

predicate :: Int -> TraceFormula -> SMTPredicate
predicate i (StateFormula s) = StatePredicate i s
predicate i (BinaryRelation Id) = IdentityPredicate i
predicate i (BinaryRelation (Sb x a)) = AssignmentPredicate i x a
predicate _ (BinaryRelation _) = BotPredicate
predicate _ (RecursiveVariable _) = BotPredicate
predicate i (Conjunction t1 t2) = ConjunctionPredicate (predicate i t1) (predicate i t2)
predicate i (Disjunction t1 t2) = DisjunctionPredicate (predicate i t1) (predicate i t2)
predicate _ (Chop _ _) = BotPredicate
predicate i m@(Mu _ _) = predicate i (unfold m)

instance SMTify SMTPredicate where
  smtify :: SMTPredicate -> Text
  smtify (StatePredicate i b) = smtify $ stateful b i -- TODO this needs to be changed to allow for postconditions
  smtify (IdentityPredicate i) = smtOp ("id" <+> indexedState i <+> indexedState (i + 1))
  smtify (AssignmentPredicate i x a) = smtOp ("sb_" <> x <+> indexedState i <+> indexedState (i + 1) <+> smtify (stateful a i))
  smtify (ConjunctionPredicate a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (DisjunctionPredicate a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify BotPredicate = "false"

  states :: SMTPredicate -> Set Int
  states (StatePredicate i _) = Set.fromList [i, i - 1]
  states (IdentityPredicate i) = Set.fromList [i, i + 1]
  states (AssignmentPredicate i _ _) = Set.fromList [i, i + 1]
  states (ConjunctionPredicate a b) = states a `Set.union` states b
  states (DisjunctionPredicate a b) = states a `Set.union` states b
  states BotPredicate = Set.empty

instance (Variables SMTPredicate) where
  variables :: SMTPredicate -> Set Text
  variables (StatePredicate _ b) = variables b
  variables (IdentityPredicate _) = Set.empty
  variables (AssignmentPredicate _ x a) = Set.insert x (variables a)
  variables (ConjunctionPredicate a b) = variables a `Set.union` variables b
  variables (DisjunctionPredicate a b) = variables a `Set.union` variables b
  variables BotPredicate = Set.empty