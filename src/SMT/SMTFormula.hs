{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.SMTFormula (SMTFormula (..), (==>), (&&&), (|||)) where

import Data.Expression (Variables (variables))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import SMT.SMTPredicate
import SMT.SMTUtil (SMTify (smtify, states), smtOp, (<+>))
import Util.PrettyUtil (bot, implies, land, lnot, lor, top)

data SMTFormula
  = Top
  | Bot
  | Implies SMTFormula SMTFormula
  | Not SMTFormula
  | And SMTFormula SMTFormula
  | Or SMTFormula SMTFormula
  | Predicate SMTPredicate
  deriving (Show)

instance Pretty SMTFormula where
  pretty :: SMTFormula -> Doc ann
  pretty Top = top
  pretty Bot = bot
  pretty (Implies a b) = parens $ pretty a P.<+> implies P.<+> pretty b
  pretty (Not a) = lnot <> pretty a
  pretty (And a b) = parens $ pretty a P.<+> land P.<+> pretty b
  pretty (Or a b) = parens $ pretty a P.<+> lor P.<+> pretty b
  pretty (Predicate p) = parens $ pretty p

infixr 1 ==>

(==>) :: SMTFormula -> SMTFormula -> SMTFormula
(==>) a b = a `Implies` b

infixr 2 &&&

(&&&) :: SMTFormula -> SMTFormula -> SMTFormula
(&&&) a b = a `And` b

infixr 2 |||

(|||) :: SMTFormula -> SMTFormula -> SMTFormula
(|||) a b = a `Or` b

instance SMTify SMTFormula where
  smtify :: SMTFormula -> Text
  smtify (Top) = "true"
  smtify (Bot) = "false"
  smtify (Implies a b) = smtOp ("=>" <+> smtify a <+> smtify b)
  smtify (Not a) = smtOp ("not" <+> smtify a)
  smtify (And a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (Or a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify (Predicate p) = smtify p

  states :: SMTFormula -> Set Int
  states Top = Set.empty
  states Bot = Set.empty
  states (Implies a b) = states a `Set.union` states b
  states (Not a) = states a
  states (And a b) = states a `Set.union` states b
  states (Or a b) = states a `Set.union` states b
  states (Predicate p) = states p

instance Variables SMTFormula where
  variables :: SMTFormula -> Set Text
  variables Top = Set.empty
  variables Bot = Set.empty
  variables (Implies a b) = variables a `Set.union` variables b
  variables (Not a) = variables a
  variables (And a b) = variables a `Set.union` variables b
  variables (Or a b) = variables a `Set.union` variables b
  variables (Predicate p) = variables p
