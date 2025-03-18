{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FTC.SMTFormula (SMTFormula (..), (==>), (&&&), (|||)) where
import Data.FTC.SMTPredicate
import Data.FOL.SMTUtil (SMTify (smtify), smtOp, (<+>))
import Data.Text (Text)

data SMTFormula
  = Top
  | Bot
  | Implies SMTFormula SMTFormula
  | Not SMTFormula
  | And SMTFormula SMTFormula
  | Or SMTFormula SMTFormula
  | Predicate SMTPredicate
  deriving (Show)

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