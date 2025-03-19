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
  )
where

import Data.Expression
import Data.Map.Strict (empty, insert)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Prettyprinter
import Data.FTC.Contract (Contract, Contracts, trueContract)
import qualified Data.Map as Map

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
            brackets (pretty pre) <> line <>
            brackets (pretty post) <> line <>
            pretty name <+> braces (line <> pretty statement <> line) <> line
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
simpleProg = Program {methods = [("m",(Not (LessThan (AVar "x") (Constant 0)),Equal (AVar "x") (Constant 0)),Condition (Not (Or (LessThan (AVar "x") (Constant 0)) (Equal (AVar "x") (Constant 0)))) (Sequence (Assignment "x" (Minus (AVar "x") (Constant 1))) (Method "m")) Skip)], main = Just (Method "m")}