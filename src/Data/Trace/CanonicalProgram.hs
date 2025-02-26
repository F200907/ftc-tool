{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Trace.CanonicalProgram (Program (..), smallStep, smallStep', bigStep, bigStep', fromTraceFormula, strongestTraceFormula) where

import Data.Expression
import Data.Map.Strict (empty, insert)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Trace.TraceLogic (BinaryRelation (Id, Sb), TraceFormula (BinaryRelation, Chop, Conjunction, Disjunction, Mu, RecursiveVariable, StateFormula))
import Prettyprinter

type MethodDefinition = (MethodName, Statement)

type MethodName = Text

data Statement
  = Skip
  | Assignment VariableName ArithmeticExpr
  | Sequence Statement Statement
  | Condition BooleanExpr Statement Statement
  | Method MethodName
  | Diverge
  | NonDeterministicChoice Statement Statement
  deriving (Show)

indent' :: Doc ann -> Doc ann
indent' = indent 2

instance Pretty Statement where
  pretty :: Statement -> Doc ann
  pretty Skip = "skip"
  pretty (Assignment x a) = pretty x <+> ":=" <+> pretty a
  pretty (Sequence s1 s2) = pretty s1 <> ";" <> line <> pretty s2
  pretty (Condition b s1 s2) = parens $ "if" <+> pretty b <+> "then" <> line <> vsep [indent' (pretty s1), "else", indent' (pretty s2)]
  pretty (Method m) = pretty m <> parens emptyDoc
  pretty Diverge = "diverge"
  pretty (NonDeterministicChoice s1 s2) = parens ("if" <+> "🞵" <+> "then" <> line <> vsep [indent' (pretty s1), "else", indent' (pretty s2)])

data Program = Program {methods :: [MethodDefinition], main :: Maybe Statement} deriving (Show)

instance Pretty Program where
  pretty :: Program -> Doc ann
  pretty (Program {methods, main}) =
    vsep $
      map
        ( \(name, statement) ->
            pretty name <+> braces (line <> pretty statement <> line) <> line
        )
        methods
        ++ map pretty (catMaybes [main])

smallStep :: Program -> [(Maybe Statement, Valuation)]
smallStep program@(Program {main}) = case main of
  Just s -> smallStep' program empty s
  Nothing -> [(Nothing, empty)]

smallStep' :: Program -> Valuation -> Statement -> [(Maybe Statement, Valuation)]
smallStep' program@(Program {methods}) v s = case s of
  Skip -> [(Nothing, v)]
  Assignment x a -> [(Nothing, insert x (evaluate a v) v)]
  Sequence s1 s2 ->
    map
      ( \case
          (Nothing, v') -> (Just s2, v')
          (Just s1', v') -> (Just (Sequence s1' s2), v')
      )
      $ smallStep' program v s1
  Condition b s1 s2 -> if evaluate b v then [(Just s1, v)] else [(Just s2, v)]
  Method m -> [(lookup m methods, v)]
  Diverge -> [(Just Diverge, v)]
  NonDeterministicChoice s1 s2 -> smallStep' program v s1 ++ smallStep' program v s2

bigStep :: Program -> [Valuation]
bigStep program@(Program {main}) = bigStep' program empty main

bigStep' :: Program -> Valuation -> Maybe Statement -> [Valuation]
bigStep' _ v Nothing = [v]
bigStep' _ v (Just Diverge) = [v]
bigStep' program v (Just s) = concatMap (\(s', v') -> bigStep' program v' s') $ smallStep' program v s

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
          let sm = fromMaybe (error $ "could not find method " ++ unpack m) (lookup m methods)
           in Chop (BinaryRelation Id) (Mu m (stf (Set.insert m methodRVars) sm))
    stf _ Diverge = Chop (BinaryRelation Id) (Mu "abort" (Chop (BinaryRelation Id) (RecursiveVariable "abort")))
    stf methodRVars (NonDeterministicChoice s1 s2) = Disjunction (Chop (BinaryRelation Id) (stf methodRVars s1)) (Chop (BinaryRelation Id) (stf methodRVars s2))

fromTraceFormula :: TraceFormula -> Program
fromTraceFormula t = let (statement, definitions) = fromTraceFormula' t in Program definitions (Just statement)
  where
    fromTraceFormula' (BinaryRelation Id) = (Skip, [])
    fromTraceFormula' (BinaryRelation (Sb x a)) = (Assignment x a, [])
    fromTraceFormula' (RecursiveVariable x) = (Method x, [])
    fromTraceFormula' (Chop t1 t2) =
      let (s1, m1) = fromTraceFormula' t1
          (s2, m2) = fromTraceFormula' t2
       in (Sequence s1 s2, m1 ++ m2)
    fromTraceFormula' (Conjunction (StateFormula p) phi) =
      let (s, m) = fromTraceFormula' phi
       in (Condition p s Diverge, m)
    fromTraceFormula' (Disjunction t1 t2) =
      let (s1, m1) = fromTraceFormula' t1
          (s2, m2) = fromTraceFormula' t2
       in (NonDeterministicChoice s1 s2, m1 ++ m2)
    fromTraceFormula' (Mu x phi) =
      let (s, m) = fromTraceFormula' phi
       in (Method x, m ++ [(x, s)])
    fromTraceFormula' _ = error "unexpected form of trace formula"

_testTranslation :: Program
_testTranslation = fromTraceFormula $ Chop (BinaryRelation (Sb "y" (Constant 0))) (Mu "x" (Disjunction (BinaryRelation Id) (Chop (BinaryRelation (Sb "y" (Plus (AVar "y") (Constant 1)))) (RecursiveVariable "x"))))