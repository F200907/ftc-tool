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
  )
where

import Data.Expression
import Data.Map.Strict (empty, insert)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, italicized)
import Util.PrettyUtil

type MethodName = Text

type MethodDefinition = (MethodName, Statement)

data Statement
  = Skip
  | Assignment VariableName ArithmeticExpr
  | Sequence Statement Statement
  | Condition BooleanExpr Statement Statement
  | Method MethodName
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

instance {-# OVERLAPS #-} PrettyAnsi Statement where
  prettyAnsi :: Statement -> Doc AnsiStyle
  prettyAnsi Skip = annotate skipStyle "skip"
  prettyAnsi (Assignment x a) = prettyAnsi x <+> ":=" <+> prettyAnsi a
  prettyAnsi (Sequence s1 s2) = prettyAnsi s1 <> ";" <> line <> prettyAnsi s2
  prettyAnsi (Condition b s1 s2) = parens $ annotate keywordStyle "if" <+> prettyAnsi b <+> annotate keywordStyle "then" <> line <> vsep [indent' (prettyAnsi s1), annotate keywordStyle "else", indent' (prettyAnsi s2)]
  prettyAnsi (Method m) = annotate italicized (prettyAnsi m) <> parens emptyDoc

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
        ( \(name, statement) ->
            pretty name <+> braces (line <> pretty statement <> line) <> line
        )
        methods
        ++ map pretty (catMaybes [main])

instance {-# OVERLAPS #-} PrettyAnsi Program where
  prettyAnsi :: Program -> Doc AnsiStyle
  prettyAnsi (Program {methods, main}) =
    vsep $
      map
        ( \(name, statement) ->
            prettyAnsi name <+> braces (line <> prettyAnsi statement <> line) <> line
        )
        methods
        ++ map prettyAnsi (catMaybes [main])

_testProgram :: Program
_testProgram = Program {methods = [("down", _testStatement), ("up", _testStatement)], main = Just _testStatement}

emptyProgram :: Program
emptyProgram = Program [] Nothing

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
  Method m -> (lookup m methods, v)

bigStep :: Program -> Valuation
bigStep program@(Program {main}) = bigStep' program empty main

bigStep' :: Program -> Valuation -> Maybe Statement -> Valuation
bigStep' _ v Nothing = v
bigStep' program v (Just s) = let (s', v') = smallStep' program v s in bigStep' program v' s'

instance (Renameable Statement) where
  rename :: Statement -> VariableName -> VariableName -> Statement
  rename Skip _ _ = Skip
  rename (Assignment y a) x x'
    | y == x = Assignment x' (rename a x x')
    | otherwise = Assignment y (rename a x x')
  rename (Sequence s1 s2) x x' = Sequence (rename s1 x x') (rename s2 x x')
  rename (Condition b s1 s2) x x' = Condition (rename b x x') (rename s1 x x') (rename s2 x x')
  rename s@(Method _) _ _ = s