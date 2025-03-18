{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FTC.SMTPredicate (SMTPredicate (..), predicate) where

import Data.Expression (ArithmeticExpr, BooleanExpr, VariableName)
import Data.FOL.SMTUtil (SMTify (..), Stateful (stateful), indexedState, smtOp, (<+>))
import Data.Text (Text)
import Data.Trace.TraceLogic (BinaryRelation (Id, Sb), TraceFormula (BinaryRelation, Chop, Conjunction, Disjunction, Mu, RecursiveVariable, StateFormula), unfold)

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
  smtify (StatePredicate i b) = smtify $ stateful b i
  smtify (IdentityPredicate i) = smtOp ("id" <+> indexedState i <+> indexedState (i + 1))
  smtify (AssignmentPredicate i x a) = smtOp ("sb_" <> x <+> indexedState i <+> indexedState (i + 1) <+> smtify (stateful a i))
  smtify (ConjunctionPredicate a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (DisjunctionPredicate a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify BotPredicate = "false"
