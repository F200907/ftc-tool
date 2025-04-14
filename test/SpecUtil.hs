module SpecUtil (genName, genInt, genArthExpr, genBoolExpr, genStmt, genTF) where

import Data.Expression (ArithmeticExpr, BooleanExpr)
import qualified Data.Expression as Expr
import Data.Text (Text, group, pack)
import Data.Trace.Program (Statement (..))
import Data.Trace.TraceLogic (BinaryRelation (..), TraceFormula (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog (MonadGen)

genName :: (MonadGen m) => m Text
genName = Gen.element $ group $ pack ['a' .. 'z']

genInt :: (MonadGen m) => m Int
genInt = Gen.int $ Range.constant 0 255

genArthExpr :: (MonadGen m) => m ArithmeticExpr
genArthExpr =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      Expr.Constant <$> genInt,
      Expr.AVar <$> genName
    ]
    [ -- recursive generators
      -- Gen.subterm genArthExpr Expr.Negation,
      Gen.subterm2 genArthExpr genArthExpr Expr.Plus,
      Gen.subterm2 genArthExpr genArthExpr Expr.Minus,
      Gen.subterm2 genArthExpr genArthExpr Expr.Times -- ,
      -- Gen.subterm2 genArthExpr genArthExpr Expr.Modulo
    ]

genBoolExpr :: (MonadGen m) => m BooleanExpr
genBoolExpr =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      pure Expr.BTrue,
      pure Expr.BFalse,
      Expr.Equal <$> genArthExpr <*> genArthExpr,
      Expr.LessThan <$> genArthExpr <*> genArthExpr
    ]
    [ -- recursive generators
      Gen.subterm genBoolExpr Expr.Not,
      Gen.subterm2 genBoolExpr genBoolExpr Expr.And,
      Gen.subterm2 genBoolExpr genBoolExpr Expr.Or
    ]

genStmt :: (MonadGen m) => m Statement
genStmt =
  Gen.recursive
    Gen.choice
    [ pure Skip,
      Assignment <$> genName <*> genArthExpr
    ]
    [ Gen.subterm2 genStmt genStmt Sequence,
      do
        cond <- Condition <$> genBoolExpr
        Gen.subterm2 genStmt genStmt cond
    ]

genTF :: (MonadGen m) => m TraceFormula
genTF =
  Gen.recursive
    Gen.choice
    [ StateFormula <$> genBoolExpr,
      pure $ BinaryRelation Id,
      BinaryRelation <$> (Sb <$> genName <*> genArthExpr),
      RecursiveVariable <$> genName
    ]
    [ Gen.subterm2 genTF genTF Conjunction,
      Gen.subterm2 genTF genTF Disjunction,
      Gen.subterm2 genTF genTF Chop,
      do
        mu <- Mu <$> genName
        Gen.subterm genTF mu
    ]