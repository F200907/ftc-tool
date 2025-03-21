{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE UndecidableInstances, OverlappingInstances #-}

module Data.Trace.Program
  ( Statement (..),
    Program (..),
    MethodDefinition,
    emptyProgram,
    smallStep',
    smallStep,
    bigStep',
    bigStep,
    rename,
    methodBody,
    lookupMethod,
    (#),
    contracts,
    contract,
    simpleProg,
    reinforce,
  )
where

import Data.Expression
import Data.FTC.Contract (Contract, Contracts, trueContract)
import qualified Data.Map as Map
import Data.Map.Strict (empty, insert)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Prettyprinter

type MethodName = Text

type MethodDefinition = (MethodName, Contract, Statement)

data Statement
  = Skip
  | Assignment VariableName ArithmeticExpr
  | Sequence Statement Statement
  | Condition BooleanExpr Statement Statement
  | Method MethodName
  deriving (Show)

(#) :: Statement -> Statement -> Statement
(#) = Sequence

indent' :: Doc ann -> Doc ann
indent' = indent 2

instance Pretty Statement where
  pretty :: Statement -> Doc ann
  pretty Skip = "skip"
  pretty (Assignment x a) = pretty x <+> ":=" <+> pretty a
  pretty (Sequence s1 s2) = pretty s1 <> ";" <> line <> pretty s2
  pretty (Condition b s1 s2) = parens $ "if" <+> pretty b <+> "then" <> line <> vsep [indent' (pretty s1), "else", indent' (pretty s2)]
  pretty (Method m) = pretty m <> parens emptyDoc

_testStatement :: Statement
_testStatement =
  Sequence
    Skip
    ( Sequence
        (Assignment "x" (Constant 0))
        (Sequence (Condition BTrue (Condition BTrue Skip Skip) (Condition BTrue Skip Skip)) (Method "m"))
    )

data Program = Program {methods :: [MethodDefinition], main :: Maybe Statement} deriving (Show)

instance Pretty Program where
  pretty :: Program -> Doc ann
  pretty (Program {methods, main}) =
    vsep $
      map
        ( \(name, (pre, post), statement) ->
            brackets (pretty pre)
              <> line
              <> brackets (pretty post)
              <> line
              <> pretty name
              <+> braces (line <> pretty statement <> line)
              <> line
        )
        methods
        ++ map pretty (catMaybes [main])

instance (Renameable Statement) where
  rename :: Statement -> VariableName -> VariableName -> Statement
  rename Skip _ _ = Skip
  rename (Assignment y a) x x'
    | y == x = Assignment x' (rename a x x')
    | otherwise = Assignment y (rename a x x')
  rename (Sequence s1 s2) x x' = Sequence (rename s1 x x') (rename s2 x x')
  rename (Condition b s1 s2) x x' = Condition (rename b x x') (rename s1 x x') (rename s2 x x')
  rename s@(Method _) _ _ = s

instance (Variables Statement) where
  variables :: Statement -> Set Text
  variables Skip = Set.empty
  variables (Assignment y a) = Set.insert y (variables a)
  variables (Sequence s1 s2) = variables s1 `Set.union` variables s2
  variables (Condition b s1 s2) = variables b `Set.union` variables s1 `Set.union` variables s2
  variables (Method _) = Set.empty

instance (Variables Program) where
  variables :: Program -> Set Text
  variables (Program {methods, main}) = foldl (\acc (_, (pre, post), s) -> variables s `Set.union` variables pre `Set.union` variables post `Set.union` acc) Set.empty methods `Set.union` maybe Set.empty variables main

_testProgram :: Program
_testProgram = Program {methods = [("down", trueContract, _testStatement), ("up", trueContract, _testStatement)], main = Just _testStatement}

emptyProgram :: Program
emptyProgram = Program [] Nothing

lookupMethod :: MethodName -> [MethodDefinition] -> Maybe Statement
lookupMethod _ [] = Nothing
lookupMethod m ((m', _, b) : xs)
  | m == m' = return b
  | otherwise = lookupMethod m xs

methodBody :: Program -> MethodName -> Maybe Statement
methodBody prog m = lookupMethod m (methods prog)

contract :: MethodName -> [MethodDefinition] -> Maybe Contract
contract _ [] = Nothing
contract m ((m', c, _) : xs)
  | m == m' = return c
  | otherwise = contract m xs

contracts :: Program -> Contracts
contracts prog = foldl (\acc (m, c, _) -> Map.insert m c acc) Map.empty (methods prog)

-- |
--  Alters contracts to add identities to variables not occurring in postconditions, and TODO strengthens the procedure contracts of non-recursive procedures.
reinforce :: Program -> Program
reinforce program@(Program {methods}) =
  let methods' = map (\(m, (pre, post), s) -> (m, (pre, post' post s), s)) methods
      post' p s =
        let ys = variables p `Set.union` variables s
            zs = xs `Set.difference` ys
            eqs = foldl (\acc z -> And acc (Equal (LVar z) (AVar z))) BTrue zs
         in And p eqs
      xs = variables program
   in program {methods = methods'}

-- SOS
smallStep :: Program -> (Maybe Statement, Valuation)
smallStep program@(Program {main}) = case main of
  Just s -> smallStep' program empty s
  Nothing -> (Nothing, empty)

smallStep' :: Program -> Valuation -> Statement -> (Maybe Statement, Valuation)
smallStep' program@(Program {methods}) v s = case s of
  Skip -> (Nothing, v)
  Assignment x a -> (Nothing, insert x (evaluate a v) v)
  Sequence s1 s2 -> case smallStep' program v s1 of
    (Nothing, v') -> (Just s2, v')
    (Just s1', v') -> (Just (Sequence s1' s2), v')
  Condition b s1 s2 -> if evaluate b v then (Just s1, v) else (Just s2, v)
  Method m -> (lookupMethod m methods, v)

bigStep :: Program -> Valuation
bigStep program@(Program {main}) = bigStep' program empty main

bigStep' :: Program -> Valuation -> Maybe Statement -> Valuation
bigStep' _ v Nothing = v
bigStep' program v (Just s) = let (s', v') = smallStep' program v s in bigStep' program v' s'

--
simpleProg :: Program
simpleProg = Program {methods = [("m", (Not (LessThan (AVar "x") (Constant 0)), Equal (AVar "x") (Constant 0)), Condition (Not (Or (LessThan (AVar "x") (Constant 0)) (Equal (AVar "x") (Constant 0)))) (Sequence (Assignment "x" (Minus (AVar "x") (Constant 1))) (Method "m")) Skip)], main = Just (Method "m")}