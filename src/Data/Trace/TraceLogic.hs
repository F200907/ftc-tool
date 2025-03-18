{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Trace.TraceLogic (TraceFormula (..), strongestTraceFormula', strongestTraceFormula, BinaryRelation (..), substitute, unfold) where

import Data.Expression
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Trace.Program (Program (Program, main, methods), Statement (Assignment, Condition, Method, Sequence, Skip), lookupMethod)
import Prettyprinter
import Util.PrettyUtil

type RecursiveVar = Text

type StateFormula = BooleanExpr

data BinaryRelation = Id | Sb VariableName ArithmeticExpr | PrePostConditions StateFormula StateFormula deriving (Show)

data TraceFormula
  = StateFormula StateFormula
  | BinaryRelation BinaryRelation
  | RecursiveVariable RecursiveVar
  | Conjunction TraceFormula TraceFormula
  | Disjunction TraceFormula TraceFormula
  | Chop TraceFormula TraceFormula
  | Mu RecursiveVar TraceFormula
  deriving (Show)

instance Pretty TraceFormula where
  pretty :: TraceFormula -> Doc ann
  pretty (StateFormula p) = pretty p
  pretty (BinaryRelation Id) = "Id"
  pretty (BinaryRelation (Sb x a)) = "Sb_" <> pretty x <> "^" <> pretty a
  pretty (BinaryRelation (PrePostConditions a b)) = brackets (pretty a <> "," <+> pretty b)
  pretty (RecursiveVariable x) = "X_" <> pretty x
  pretty (Conjunction a b) = parens (pretty a <+> land <+> pretty b)
  pretty (Disjunction a b) = parens (pretty a <+> lor <+> pretty b)
  pretty (Chop a b) = pretty a <+> arc <+> pretty b
  pretty (Mu x phi) = mu <> "X_" <> pretty x <> "." <> parens (pretty phi)

_testTrace :: TraceFormula
_testTrace = Chop (BinaryRelation Id) (Mu "even" (Disjunction (Conjunction (StateFormula (Equal (AVar "x") (Constant 0))) (Chop (BinaryRelation Id) (BinaryRelation (Sb "y" (Constant 1))))) (Conjunction (StateFormula (Not (Equal (AVar "x") (Constant 0)))) (Chop (BinaryRelation Id) (Chop (BinaryRelation (Sb "x" (Minus (AVar "x") (Constant 1)))) (Chop (BinaryRelation Id) (Mu "odd" (Disjunction (Conjunction (StateFormula (Equal (AVar "x") (Constant 0))) (Chop (BinaryRelation Id) (BinaryRelation (Sb "y" (Constant 0))))) (Conjunction (StateFormula (Not (Equal (AVar "x") (Constant 0)))) (Chop (BinaryRelation Id) (Chop (BinaryRelation (Sb "x" (Minus (AVar "x") (Constant 1)))) (Chop (BinaryRelation Id) (RecursiveVariable "even")))))))))))))

strongestTraceFormula :: Program -> TraceFormula
strongestTraceFormula program@(Program {main}) = case main of
  Just s -> strongestTraceFormula' program s
  Nothing -> BinaryRelation Id

strongestTraceFormula' :: Program -> Statement -> TraceFormula
strongestTraceFormula' (Program {methods}) = stf Set.empty
  where
    stf _ Skip = BinaryRelation Id
    stf _ (Assignment x a) = BinaryRelation (Sb x a)
    stf methodRVars (Sequence s1 s2) = Chop (stf methodRVars s1) (stf methodRVars s2)
    stf methodRVars (Condition b s1 s2) =
      Disjunction
        (Conjunction (StateFormula b) (Chop (BinaryRelation Id) (stf methodRVars s1)))
        (Conjunction (StateFormula (Not b)) (Chop (BinaryRelation Id) (stf methodRVars s2)))
    stf methodRVars (Method m)
      | Set.member m methodRVars = Chop (BinaryRelation Id) (RecursiveVariable m)
      | otherwise =
          let sm = fromMaybe (error $ "could not find method " ++ unpack m) (lookupMethod m methods)
           in Chop (BinaryRelation Id) (Mu m (stf (Set.insert m methodRVars) sm))

instance (Substitutable TraceFormula RecursiveVar TraceFormula) where
  substitute :: TraceFormula -> RecursiveVar -> TraceFormula -> TraceFormula
  substitute f@(StateFormula _) _ _ = f
  substitute f@(BinaryRelation _) _ _ = f
  substitute f@(RecursiveVariable r) x x'
    | r == x = x'
    | otherwise = f
  substitute (Conjunction a b) x x' = Conjunction (substitute a x x') (substitute b x x')
  substitute (Disjunction a b) x x' = Disjunction (substitute a x x') (substitute b x x')
  substitute (Chop a b) x x' = Chop (substitute a x x') (substitute b x x')
  substitute f@(Mu r phi) x x'
    | r == x = f
    | otherwise = Mu r (substitute phi x x')

instance (Renameable TraceFormula) where
  rename :: TraceFormula -> VariableName -> VariableName -> TraceFormula
  rename (StateFormula p) x x' = StateFormula $ substitute p x (AVar x')
  rename (BinaryRelation (Sb y a)) x x'
    | y == x = BinaryRelation $ Sb x $ substitute a x (AVar x')
    | otherwise = BinaryRelation $ Sb y $ substitute a x (AVar x')
  rename f@(BinaryRelation Id) _ _ = f
  rename (BinaryRelation (PrePostConditions a b)) x x' = BinaryRelation (PrePostConditions (substitute a x (AVar x')) (substitute b x (AVar x')))
  rename f@(RecursiveVariable _) _ _ = f
  rename (Conjunction a b) x x' = Conjunction (rename a x x') (rename b x x')
  rename (Disjunction a b) x x' = Disjunction (rename a x x') (rename b x x')
  rename (Chop a b) x x' = Chop (rename a x x') (rename b x x')
  rename (Mu r phi) x x' = Mu r (rename phi x x')

unfold :: TraceFormula -> TraceFormula
unfold m@(Mu x f) = substitute f x m
unfold t = t