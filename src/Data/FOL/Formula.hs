{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FOL.Formula (Formula (..), prettySMT) where

import Data.Expression (ArithmeticExpr (AVar), BooleanExpr, VariableName, rename)
import qualified Data.Expression as Expr
import Data.Trace.Program (Statement (Assignment, Condition, Sequence, Skip))
import Data.Trace.TraceLogic
import Prettyprinter (Doc, Pretty (pretty), parens, (<+>))
import Util.PrettyUtil

data Formula
  = Forall VariableName Formula
  | Exists VariableName Formula
  | Top
  | Bot
  | Implies Formula Formula
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Equal ArithmeticExpr ArithmeticExpr
  | LessThan ArithmeticExpr ArithmeticExpr
  deriving (Show)

instance Pretty Formula where
  pretty :: Formula -> Doc ann
  pretty (Forall x a) = parens (forAll <+> pretty x <> "." <+> pretty a)
  pretty (Exists x a) = parens (exists <+> pretty x <> "." <+> pretty a)
  pretty Top = top
  pretty Bot = bot
  pretty (Implies a b) = parens (pretty a <+> implies <+> pretty b)
  pretty (Not a) = lnot <> pretty a
  pretty (And a b) = parens (pretty a <+> land <+> pretty b)
  pretty (Or a b) = parens (pretty a <+> lor <+> pretty b)
  pretty (Equal a b) = parens (pretty a <+> "=" <+> pretty b)
  pretty (LessThan a b) = parens (pretty a <+> "<" <+> pretty b)

prettySMT' :: ArithmeticExpr -> Doc ann
prettySMT' (Expr.Constant c) = pretty c
prettySMT' (Expr.AVar x) = pretty x
prettySMT' (Expr.Negation a) = "-" <> prettySMT' a
prettySMT' (Expr.Plus a b) = parens ("+" <+> prettySMT' a <+> prettySMT' b)
prettySMT' (Expr.Minus a b) = parens ("-" <+> prettySMT' a <+> prettySMT' b)
prettySMT' (Expr.Times a b) = parens ("*" <+> prettySMT' a <+> prettySMT' b)

prettySMT :: Formula -> Doc ann
prettySMT (Forall x f) = parens ("forall " <+> parens (parens (pretty x <+> "Int")) <+> prettySMT f)
prettySMT (Exists x f) = parens ("exists " <+> parens (parens (pretty x <+> "Int")) <+> prettySMT f)
prettySMT Top = "true"
prettySMT Bot = "false"
prettySMT (Implies a b) = parens ("=>" <+> prettySMT a <+> prettySMT b)
prettySMT (Not f) = parens ("not" <+> prettySMT f)
prettySMT (And a b) = parens ("and" <+> prettySMT a <+> prettySMT b)
prettySMT (Or a b) = parens ("or" <+> prettySMT a <+> prettySMT b)
prettySMT (Equal a b) = parens ("=" <+> prettySMT' a <+> prettySMT' b)
prettySMT (LessThan a b) = parens ("<" <+> prettySMT' a <+> prettySMT' b)

_testPrint :: Doc ann
_testPrint = pretty $ Implies Top (Forall "x" (Exists "y" (LessThan (AVar "x") (AVar "y"))))

fromBooleanExpr :: BooleanExpr -> Formula
fromBooleanExpr Expr.BTrue = Top
fromBooleanExpr Expr.BFalse = Bot
fromBooleanExpr (Expr.Not b) = Not (fromBooleanExpr b)
fromBooleanExpr (Expr.And a b) = And (fromBooleanExpr a) (fromBooleanExpr b)
fromBooleanExpr (Expr.Or a b) = Or (fromBooleanExpr a) (fromBooleanExpr b)
fromBooleanExpr (Expr.Equal a b) = Equal a b
fromBooleanExpr (Expr.LessThan a b) = LessThan a b

predicateFresh :: TraceFormula -> VariableName -> VariableName -> Formula
predicateFresh (StateFormula p) x x' = fromBooleanExpr $ substitute p x (AVar x')
predicateFresh (BinaryRelation Id) _ _ = Top
predicateFresh (BinaryRelation (Sb y a)) x x'
  | y == x = Equal (AVar x') a
  | otherwise = Equal (AVar y) a
predicateFresh (BinaryRelation _) _ _ = Bot
predicateFresh (RecursiveVariable _) _ _ = Bot
predicateFresh (Conjunction a b) x x' = And (predicateFresh a x x') (predicateFresh b x x')
predicateFresh (Disjunction a b) x x' = Or (predicateFresh a x x') (predicateFresh b x x')
predicateFresh (Chop _ _) _ _ = Bot
predicateFresh f@(Mu r phi) x x' = predicateFresh (substitute phi r f) x x'

predicate :: TraceFormula -> Formula
predicate t = predicateFresh t "" ""

predicateMaybe :: Maybe TraceFormula -> Formula
predicateMaybe f = predicateFreshMaybe f "" ""

predicateFreshMaybe :: Maybe TraceFormula -> VariableName -> VariableName -> Formula
predicateFreshMaybe (Just f) x x' = predicateFresh f x x'
predicateFreshMaybe Nothing _ _ = Bot

headTF :: TraceFormula -> Maybe TraceFormula
headTF (Chop a _) = return a
headTF _ = Nothing

tailTF :: TraceFormula -> Maybe TraceFormula
tailTF (Chop _ b) = return b
tailTF _ = Nothing

ftc :: Maybe Statement -> Maybe TraceFormula -> Formula
ftc Nothing Nothing = Top
ftc _ Nothing = Bot
ftc Nothing _ = Bot
ftc s'@(Just s) (Just phi) = case phi of
  Disjunction a b -> Or (ftc s' (Just a)) (ftc s' (Just b))
  Conjunction a b -> And (ftc s' (Just a)) (ftc s' (Just b))
  f@(Mu x psi) -> ftc s' (Just (substitute psi x f))
  f -> ftc' s f

ftc' :: Statement -> TraceFormula -> Formula
ftc' (Assignment x a) f = Implies (Equal (AVar (x <> "'")) a) (predicateFresh f x (x <> "'"))
ftc' (Sequence (Assignment x a) s) f =
  Implies
    (Equal (AVar (x <> "'")) a)
    ( And
        (predicateFreshMaybe (headTF f) x (x <> "'"))
        (ftc (Just (rename s x (x <> "'"))) t)
    )
  where
    t = case tailTF f of
      Just t' -> return $ rename t' x (x <> "'")
      Nothing -> Nothing
ftc' Skip f = predicate f
ftc' (Sequence Skip s) f = And (predicateMaybe (headTF f)) (ftc (Just s) (tailTF f))
ftc' (Condition b s1 s2) f =
  And
    (predicateMaybe (headTF f))
    ( And
        (Implies (fromBooleanExpr b) (ftc (Just s1) (tailTF f)))
        (Implies (Not (fromBooleanExpr b)) (ftc (Just s2) (tailTF f)))
    )
ftc' (Sequence (Condition b s1 s2) s) f =
  And
    (predicateMaybe (headTF f))
    ( And
        (Implies (fromBooleanExpr b) (ftc (Just (Sequence s1 s)) (tailTF f)))
        (Implies (Not (fromBooleanExpr b)) (ftc (Just (Sequence s2 s)) (tailTF f)))
    )
ftc' _ _ = undefined

_testFTC :: Formula
_testFTC = ftc (Just (Sequence (Assignment "x" (Expr.Plus (AVar "x") (Expr.Constant 1))) (Assignment "y" (Expr.Plus (AVar "x") (Expr.Constant 1))))) (Just (Chop (BinaryRelation (Sb "x" (Expr.Constant 1))) (BinaryRelation (Sb "y" (Expr.Constant 2)))))

_testFTC2 :: Formula
_testFTC2 = ftc (Just (Sequence (Assignment "x" (Expr.Plus (AVar "x") (Expr.Constant 1))) (Assignment "y" (Expr.Plus (AVar "x") (Expr.Constant 1))))) (Just (Mu "X" (Disjunction (StateFormula (Expr.LessThan (Expr.Constant 0) (AVar "x"))) (Chop (StateFormula (Expr.LessThan (Expr.Constant 0) (AVar "x"))) (RecursiveVariable "X")))))