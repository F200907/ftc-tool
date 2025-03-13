{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FOL.SMTUtil (StateType (..), genState, SMTify (..), Stateful (..), indexedState, (<+>), smtOp) where

import Data.Expression
import Data.List (delete, intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as Text

newtype StateType = StateType [Text]

genState :: StateType -> Text
genState (StateType xs) = declareDatatype <> "\n" <> idPred <> "\n" <> foldl (\acc x -> acc <> sbPred x <> "\n") "" xs
  where
    declareDatatype = "(declare-datatypes ((State 0)) (((mk-state " <> concatWithSpace declareDatatype' <> "))))"
    declareDatatype' = map (\x -> "(" <> x <> " Int)") xs
    idPred = "(define-fun id ((s1 State) (s2 State)) Bool (and " <> concatWithSpace (equivs xs) <> "))"
    sbPred x = "(define-fun sb_" <> x <> " ((s1 State) (s2 State) (val Int)) Bool (and " <> concatWithSpace ("(= (" <> x <> " s2) val)" : equivs (delete x xs)) <> "))"
    equivs = map (\x -> "(= (" <> x <> " s1) (" <> x <> " s2))")
    concatWithSpace = Text.concat . intersperse " "

indexedState :: Int -> Text
indexedState i = "state" <> pack (show i)

class Stateful a where
  stateful :: a -> Int -> a

instance (Stateful ArithmeticExpr) where
  stateful :: ArithmeticExpr -> Int -> ArithmeticExpr
  stateful (Constant c) _ = Constant c
  stateful (AVar x) i = AVar $ smtOp (x <+> indexedState i)
  stateful (Negation a) i = Negation (stateful a i)
  stateful (Plus a b) i = Plus (stateful a i) (stateful b i)
  stateful (Minus a b) i = Minus (stateful a i) (stateful b i)
  stateful (Times a b) i = Times (stateful a i) (stateful b i)

instance (Stateful BooleanExpr) where
  stateful :: BooleanExpr -> Int -> BooleanExpr
  stateful (Not b) i = Not (stateful b i)
  stateful (And a b) i = And (stateful a i) (stateful b i)
  stateful (Or a b) i = Or (stateful a i) (stateful b i)
  stateful (Equal a b) i = Equal (stateful a i) (stateful b i)
  stateful (LessThan a b) i = LessThan (stateful a i) (stateful b i)
  stateful b _ = b

(<+>) :: Text -> Text -> Text
(<+>) a b = a <> " " <> b

smtOp :: Text -> Text
smtOp t = "(" <> t <> ")"

class SMTify a where
  smtify :: a -> Text

instance (SMTify ArithmeticExpr) where
  smtify :: ArithmeticExpr -> Text
  smtify (Constant c) = pack (show c)
  smtify (AVar x) = x
  smtify (Negation a) = "-" <> smtify a
  smtify (Plus a b) = smtOp ("+" <+> smtify a <+> smtify b)
  smtify (Minus a b) = smtOp ("-" <+> smtify a <+> smtify b)
  smtify (Times a b) = smtOp ("*" <+> smtify a <+> smtify b)

instance (SMTify BooleanExpr) where
  smtify BTrue = "true"
  smtify BFalse = "false"
  smtify (Not f) = smtOp ("not" <+> smtify f)
  smtify (And a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (Or a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify (Equal a b) = smtOp ("=" <+> smtify a <+> smtify b)
  smtify (LessThan a b) = smtOp ("<" <+> smtify a <+> smtify b)
