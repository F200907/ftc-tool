{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.Formula (SMTFormula (..), (==>), (&&&), (|||), predicate) where

import Data.Expression (ArithmeticExpr, BooleanExpr, VariableName)
import qualified Data.Expression as Exp
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import SMT.SMTUtil (SMTify (smtify, states), indexedState, smtOp, (<+>))
import Util.PrettyUtil (bot, implies, land, lnot, lor, top)
import Data.Trace.TraceLogic (TraceFormula (..), BinaryRelation (..), unfold)
import Data.Variable

data SMTFormula
  = Top
  | Bot
  | Implies SMTFormula SMTFormula
  | Not SMTFormula
  | And SMTFormula SMTFormula
  | Or SMTFormula SMTFormula
  | StatePredicate Int BooleanExpr
  | BinaryPredicate Int Int BooleanExpr
  | IdentityPredicate Int
  | AssignmentPredicate Int VariableName ArithmeticExpr
  deriving (Show)

instance Pretty SMTFormula where
  pretty :: SMTFormula -> Doc ann
  pretty Top = top
  pretty Bot = bot
  pretty (Implies a b) = parens $ pretty a P.<+> implies P.<+> pretty b
  pretty (Not a) = lnot <> pretty a
  pretty (And a b) = parens $ pretty a P.<+> land P.<+> pretty b
  pretty (Or a b) = parens $ pretty a P.<+> lor P.<+> pretty b
  pretty (StatePredicate i b) = parens $ pretty b <> parens (pretty i <> "," P.<+> pretty (i + 1))
  pretty (BinaryPredicate i j b) = parens $ pretty b <> parens (pretty i <> "," P.<+> pretty j)
  pretty (IdentityPredicate i) = parens $ "Id" <> parens (pretty i <> "," P.<+> pretty (i + 1))
  pretty (AssignmentPredicate i x a) = parens $ "Sb_" <> pretty x <> "^" <> pretty a <> parens (pretty i <> "," P.<+> pretty (i + 1))

infixr 1 ==>

(==>) :: SMTFormula -> SMTFormula -> SMTFormula
(==>) a b = a `Implies` b

infixr 2 &&&

(&&&) :: SMTFormula -> SMTFormula -> SMTFormula
(&&&) a b = a `And` b

infixr 2 |||

(|||) :: SMTFormula -> SMTFormula -> SMTFormula
(|||) a b = a `Or` b

predicate :: Int -> TraceFormula -> SMTFormula
predicate i (StateFormula s) = StatePredicate i s
predicate i (BinaryRelation Id) = IdentityPredicate i
predicate i (BinaryRelation (Sb x a)) = AssignmentPredicate i x a
predicate _ (BinaryRelation _) = Bot
predicate _ (RecursiveVariable _) = Bot
predicate i (Conjunction t1 t2) = And (predicate i t1) (predicate i t2)
predicate i (Disjunction t1 t2) = Or (predicate i t1) (predicate i t2)
predicate _ (Chop _ _) = Bot
predicate i m@(Mu _ _) = predicate i (unfold m)

instance SMTify SMTFormula where
  smtify :: SMTFormula -> Text
  smtify (Top) = "true"
  smtify (Bot) = "false"
  smtify (Implies a b) = smtOp ("=>" <+> smtify a <+> smtify b)
  smtify (Not a) = smtOp ("not" <+> smtify a)
  smtify (And a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (Or a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify (StatePredicate i b) = smtify $ stateful b i i
  smtify (BinaryPredicate i j b) = smtify $ stateful b i j
  smtify (IdentityPredicate i) = smtOp ("id" <+> indexedState i <+> indexedState (i + 1))
  smtify (AssignmentPredicate i x a) = smtOp ("sb_" <> x <+> indexedState i <+> indexedState (i + 1) <+> smtify (stateful a i i))

  states :: SMTFormula -> Set Int
  states Top = Set.empty
  states Bot = Set.empty
  states (Implies a b) = states a `Set.union` states b
  states (Not a) = states a
  states (And a b) = states a `Set.union` states b
  states (Or a b) = states a `Set.union` states b
  states (StatePredicate i _) = Set.fromList [i, i - 1]
  states (BinaryPredicate i j _) = Set.fromList [i, j]
  states (IdentityPredicate i) = Set.fromList [i, i + 1]
  states (AssignmentPredicate i _ _) = Set.fromList [i, i + 1]

instance Variables SMTFormula where
  variables :: SMTFormula -> Set Text
  variables Top = Set.empty
  variables Bot = Set.empty
  variables (Implies a b) = variables a `Set.union` variables b
  variables (Not a) = variables a
  variables (And a b) = variables a `Set.union` variables b
  variables (Or a b) = variables a `Set.union` variables b
  variables (StatePredicate _ b) = variables b
  variables (BinaryPredicate _ _ b) = variables b
  variables (IdentityPredicate _) = Set.empty
  variables (AssignmentPredicate _ x a) = Set.insert x (variables a)

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