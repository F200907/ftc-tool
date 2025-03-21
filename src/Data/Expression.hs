{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Expression (VariableName, ArithmeticExpr (..), BooleanExpr (..), Valuation, Renameable (..), Substitutable (..), Evaluable (..), Variables (..)) where

import Data.Map.Strict (Map, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    parens,
    (<+>),
  )
import Util.PrettyUtil
import Prelude hiding (lookup)

type VariableName = Text

type Valuation = Map VariableName Int

data ArithmeticExpr
  = Constant Int
  | AVar VariableName
  | LVar VariableName
  | Negation ArithmeticExpr
  | Plus ArithmeticExpr ArithmeticExpr
  | Minus ArithmeticExpr ArithmeticExpr
  | Times ArithmeticExpr ArithmeticExpr
  | Modulo ArithmeticExpr ArithmeticExpr
  deriving (Show)

instance Pretty ArithmeticExpr where
  pretty :: ArithmeticExpr -> Doc ann
  pretty (Constant c)
    | c >= 0 = pretty c
    | otherwise = parens (pretty c)
  pretty (AVar x) = pretty x
  pretty (LVar x) = "#" <> pretty x
  pretty (Negation (AVar x)) = parens ("-" <> pretty x)
  pretty (Negation (Constant c)) = parens ("-" <> pretty c)
  pretty (Negation a) = parens ("-" <> parens (pretty a))
  pretty (Plus a b) = parens (pretty a <+> "+" <+> pretty b)
  pretty (Minus a b) = parens (pretty a <+> "-" <+> pretty b)
  pretty (Times a b) = parens (pretty a <+> "*" <+> pretty b)
  pretty (Modulo a b) = parens (pretty a <+> "%" <+> pretty b)

data BooleanExpr
  = BTrue
  | BFalse
  | Not BooleanExpr
  | And BooleanExpr BooleanExpr
  | Or BooleanExpr BooleanExpr
  | Equal ArithmeticExpr ArithmeticExpr
  | LessThan ArithmeticExpr ArithmeticExpr
  deriving (Show)

instance Pretty BooleanExpr where
  pretty :: BooleanExpr -> Doc ann
  pretty BTrue = top
  pretty BFalse = bot
  pretty (Not a) = lnot <> pretty a
  pretty (And a b) = parens (pretty a <+> land <+> pretty b)
  pretty (Or (Not a) b) = parens (pretty a <+> implies <+> pretty b)
  pretty (Or a b) = parens (pretty a <+> lor <+> pretty b)
  pretty (Equal a b) = parens (pretty a <+> "=" <+> pretty b)
  pretty (LessThan a b) = parens (pretty a <+> "<" <+> pretty b)

_testExpression :: BooleanExpr
_testExpression = And BTrue (And (Not BFalse) (And (Equal (Plus (Constant 0) (AVar "x")) (Minus (Constant 1) (Constant (-7)))) (LessThan (Times (AVar "y") (AVar "y")) (Constant 19))))

class Variables a where
  variables :: a -> Set Text

instance (Variables ArithmeticExpr) where
  variables :: ArithmeticExpr -> Set Text
  variables (Constant _) = Set.empty
  variables (AVar x) = Set.singleton x
  variables (LVar x) = Set.singleton x
  variables (Negation a) = variables a
  variables (Plus a b) = variables a `Set.union` variables b
  variables (Minus a b) = variables a `Set.union` variables b
  variables (Times a b) = variables a `Set.union` variables b
  variables (Modulo a b) = variables a `Set.union` variables b

instance (Variables BooleanExpr) where
  variables :: BooleanExpr -> Set Text
  variables BTrue = Set.empty
  variables BFalse = Set.empty
  variables (Not a) = variables a
  variables (And a b) = variables a `Set.union` variables b
  variables (Or a b) = variables a `Set.union` variables b
  variables (Equal a b) = variables a `Set.union` variables b
  variables (LessThan a b) = variables a `Set.union` variables b

class Evaluable a b where
  evaluate :: a -> Valuation -> b

instance (Evaluable ArithmeticExpr Int) where
  evaluate :: ArithmeticExpr -> Valuation -> Int
  evaluate (Constant c) _ = c
  evaluate (AVar x) v = fromMaybe (error $ "couldn't find value for " ++ unpack x) (lookup x v)
  evaluate (LVar x) v = fromMaybe (error $ "couldn't find value for " ++ unpack x) (lookup x v)
  evaluate (Negation a) v = -(evaluate a v)
  evaluate (Plus a b) v = evaluate a v + evaluate b v
  evaluate (Minus a b) v = evaluate a v - evaluate b v
  evaluate (Times a b) v = evaluate a v * evaluate b v
  evaluate (Modulo a b) v = evaluate a v `mod` evaluate b v

instance (Evaluable BooleanExpr Bool) where
  evaluate :: BooleanExpr -> Valuation -> Bool
  evaluate BTrue _ = True
  evaluate BFalse _ = False
  evaluate (Not a) v = not (evaluate a v)
  evaluate (And a b) v = evaluate a v && evaluate b v
  evaluate (Or a b) v = evaluate a v || evaluate b v
  evaluate (Equal a b) v = (evaluate a v :: Int) == evaluate b v
  evaluate (LessThan a b) v = (evaluate a v :: Int) < evaluate b v

class Substitutable a b c where
  substitute :: a -> b -> c -> a

instance (Substitutable ArithmeticExpr VariableName ArithmeticExpr) where
  substitute :: ArithmeticExpr -> VariableName -> ArithmeticExpr -> ArithmeticExpr
  substitute c@(Constant _) _ _ = c
  substitute a@(AVar v) x x'
    | v == x = x'
    | otherwise = a
  substitute a@(LVar v) x x'
    | v == x = x'
    | otherwise = a
  substitute (Negation a) x x' = Negation (substitute a x x')
  substitute (Plus a b) x x' = Plus (substitute a x x') (substitute b x x')
  substitute (Minus a b) x x' = Minus (substitute a x x') (substitute b x x')
  substitute (Times a b) x x' = Times (substitute a x x') (substitute b x x')
  substitute (Modulo a b) x x' = Modulo (substitute a x x') (substitute b x x')

instance (Substitutable BooleanExpr VariableName ArithmeticExpr) where
  substitute :: BooleanExpr -> VariableName -> ArithmeticExpr -> BooleanExpr
  substitute (Not b) x x' = Not (substitute b x x')
  substitute (And a b) x x' = And (substitute a x x') (substitute b x x')
  substitute (Or a b) x x' = Or (substitute a x x') (substitute b x x')
  substitute (Equal a b) x x' = Equal (substitute a x x') (substitute b x x')
  substitute (LessThan a b) x x' = LessThan (substitute a x x') (substitute b x x')
  substitute b _ _ = b

class Renameable a where
  rename :: a -> VariableName -> VariableName -> a

instance (Renameable ArithmeticExpr) where
  rename :: ArithmeticExpr -> VariableName -> VariableName -> ArithmeticExpr
  rename a x x' = substitute a x (AVar x')

instance (Renameable BooleanExpr) where
  rename :: BooleanExpr -> VariableName -> VariableName -> BooleanExpr
  rename b x x' = substitute b x (AVar x')
