{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.SMTUtil (genState, SMTify (..), indexedState, (<+>), smtOp) where

import Data.Expression (ArithmeticExpr (..), BooleanExpr (..))
import Data.List (delete, intersperse)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text

genState :: [Text] -> Text
genState xs = declareDatatype <> "\n" <> idPred <> "\n" <> foldl (\acc x -> acc <> sbPred x <> "\n") "" xs
  where
    declareDatatype = "(declare-datatypes ((State 0)) (((mk-state " <> concatWithSpace declareDatatype' <> "))))"
    declareDatatype' = map (\x -> "(" <> x <> " Int)") xs
    idPred = "(define-fun id ((s1 State) (s2 State)) Bool (and " <> concatWithSpace (equivs xs) <> "))"
    sbPred x = "(define-fun sb_" <> x <> " ((s1 State) (s2 State) (val Int)) Bool (and " <> concatWithSpace ("(= (" <> x <> " s2) val)" : equivs (delete x xs)) <> "))"
    equivs = map (\x -> "(= (" <> x <> " s1) (" <> x <> " s2))")
    concatWithSpace = Text.concat . intersperse " "

indexedState :: Int -> Text
indexedState i = "state" <> pack (show i)

(<+>) :: Text -> Text -> Text
(<+>) a b = a <> " " <> b

smtOp :: Text -> Text
smtOp t = "(" <> t <> ")"

class SMTify a where
  smtify :: a -> Text
  states :: a -> Set Int

instance (SMTify ArithmeticExpr) where
  smtify :: ArithmeticExpr -> Text
  smtify (Constant c) = pack (show c)
  smtify (AVar x) = x
  smtify (LVar x) = x
  smtify (Negation c@(Constant _)) = "-" <> smtify c
  smtify (Negation a) = smtOp ("-" <> smtify a)
  smtify (Plus a b) = smtOp ("+" <+> smtify a <+> smtify b)
  smtify (Minus a b) = smtOp ("-" <+> smtify a <+> smtify b)
  smtify (Times a b) = smtOp ("*" <+> smtify a <+> smtify b)

  states :: ArithmeticExpr -> Set Int
  states = const Set.empty

instance (SMTify BooleanExpr) where
  smtify :: BooleanExpr -> Text
  smtify BTrue = "true"
  smtify BFalse = "false"
  smtify (Not f) = smtOp ("not" <+> smtify f)
  smtify (And a b) = smtOp ("and" <+> smtify a <+> smtify b)
  smtify (Or a b) = smtOp ("or" <+> smtify a <+> smtify b)
  smtify (Equal a b) = smtOp ("=" <+> smtify a <+> smtify b)
  smtify (LessThan a b) = smtOp ("<" <+> smtify a <+> smtify b)

  states :: BooleanExpr -> Set Int
  states = const Set.empty
