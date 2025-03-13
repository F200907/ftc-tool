module Data.FTC.SMTFormula (SMTFormula (..), (==>), (&&&), (|||)) where
import Data.FTC.SMTPredicate

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

