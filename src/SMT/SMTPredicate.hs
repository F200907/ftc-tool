{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.SMTPredicate (SMTPredicate (..), predicate) where

import Data.Expression (ArithmeticExpr, BooleanExpr, VariableName, Variables (variables))
import qualified Data.Expression as Exp
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Trace.TraceLogic (BinaryRelation (Id, Sb), TraceFormula (BinaryRelation, Chop, Conjunction, Disjunction, Mu, RecursiveVariable, StateFormula), unfold)
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import SMT.SMTUtil (SMTify (..), indexedState, smtOp, (<+>))
import Util.PrettyUtil (land, lor)

data SMTPredicate
  = StatePredicate Int BooleanExpr
  | BinaryPredicate Int Int BooleanExpr
  | IdentityPredicate Int
  | AssignmentPredicate Int VariableName ArithmeticExpr
  | ConjunctionPredicate SMTPredicate SMTPredicate
  | DisjunctionPredicate SMTPredicate SMTPredicate
  | BotPredicate
  deriving (Show)

instance Pretty SMTPredicate where
  pretty :: SMTPredicate -> Doc ann
  pretty (StatePredicate i b) = pretty b <> parens (pretty i <> "," P.<+> pretty (i + 1))
  pretty (BinaryPredicate i j b) = pretty b <> parens (pretty i <> "," P.<+> pretty j)
  pretty (IdentityPredicate i) = "Id" <> parens (pretty i <> "," P.<+> pretty (i + 1))
  pretty (AssignmentPredicate i x a) = "Sb_" <> pretty x <> "^" <> pretty a <> parens (pretty i <> "," P.<+> pretty (i + 1))
  pretty (ConjunctionPredicate a b) = parens (pretty a P.<+> land P.<+> pretty b)
  pretty (DisjunctionPredicate a b) = parens (pretty a P.<+> lor P.<+> pretty b)
  pretty BotPredicate = "false"

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
  smtify (StatePredicate i b) = smtify $ stateful b i i
  smtify (BinaryPredicate i j b) = smtify $ stateful b i j
  smtify (IdentityPredicate i) = smtOp ("id" <+> indexedState i <+> indexedState (i + 1))
  smtify (AssignmentPredicate i x a) = smtOp ("sb_" <> x <+> indexedState i <+> indexedState (i + 1) <+> smtify (stateful a i i))
  smtify (ConjunctionPredicate a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (DisjunctionPredicate a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify BotPredicate = "false"

  -- Overestimates the states that could be reached
  states :: SMTPredicate -> Set Int
  states (StatePredicate i _) = Set.fromList [i, i - 1]
  states (BinaryPredicate i j _) = Set.fromList [i, j]
  states (IdentityPredicate i) = Set.fromList [i, i + 1]
  states (AssignmentPredicate i _ _) = Set.fromList [i, i + 1]
  states (ConjunctionPredicate a b) = states a `Set.union` states b
  states (DisjunctionPredicate a b) = states a `Set.union` states b
  states BotPredicate = Set.empty

instance (Variables SMTPredicate) where
  variables :: SMTPredicate -> Set Text
  variables (StatePredicate _ b) = variables b
  variables (BinaryPredicate _ _ b) = variables b
  variables (IdentityPredicate _) = Set.empty
  variables (AssignmentPredicate _ x a) = Set.insert x (variables a)
  variables (ConjunctionPredicate a b) = variables a `Set.union` variables b
  variables (DisjunctionPredicate a b) = variables a `Set.union` variables b
  variables BotPredicate = Set.empty

class Stateful a where
  stateful :: a -> Int -> Int -> a

instance (Stateful ArithmeticExpr) where
  stateful :: ArithmeticExpr -> Int -> Int -> ArithmeticExpr
  stateful (Exp.Constant c) _ _ = Exp.Constant c
  stateful (Exp.AVar x) _ j = Exp.AVar $ smtOp (x <+> indexedState j)
  stateful (Exp.LVar x) i _ = Exp.LVar $ smtOp (x <+> indexedState i)
  stateful (Exp.Negation a) i j = Exp.Negation (stateful a i j)
  stateful (Exp.Plus a b) i j = Exp.Plus (stateful a i j) (stateful b i j)
  stateful (Exp.Minus a b) i j = Exp.Minus (stateful a i j) (stateful b i j)
  stateful (Exp.Times a b) i j = Exp.Times (stateful a i j) (stateful b i j)
  stateful (Exp.Modulo a b) i j = Exp.Modulo (stateful a i j) (stateful b i j)

instance (Stateful BooleanExpr) where
  stateful :: BooleanExpr -> Int -> Int -> BooleanExpr
  stateful (Exp.Not b) i j = Exp.Not (stateful b i j)
  stateful (Exp.And a b) i j = Exp.And (stateful a i j) (stateful b i j)
  stateful (Exp.Or a b) i j = Exp.Or (stateful a i j) (stateful b i j)
  stateful (Exp.Equal a b) i j = Exp.Equal (stateful a i j) (stateful b i j)
  stateful (Exp.LessThan a b) i j = Exp.LessThan (stateful a i j) (stateful b i j)
  stateful Exp.BTrue _ _ = Exp.BTrue
  stateful Exp.BFalse _ _ = Exp.BFalse