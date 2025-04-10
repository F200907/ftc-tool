{-# LANGUAGE OverloadedStrings #-}

module STFSpec (spec) where

import Data.Expression (ArithmeticExpr, BooleanExpr)
import qualified Data.Expression as Expr
import Data.Text (Text, group, pack, unpack)
import Data.Trace.Program (Statement (..), Program (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Hedgehog (MonadGen, modifyMaxSuccess, hedgehog, forAll, (===), modifyMaxShrinks)
import SMT.Solver (ftcCondition, withDebug, z3, checkValidity, Validity (Valid))
import Data.Trace.TraceLogic (strongestTraceFormula)
import Control.Monad.IO.Class (liftIO)
import Prettyprinter (Pretty(pretty))
import Data.Trace.Normalise (Normalisable(normalise))
import Control.Exception (try, evaluate, SomeException (SomeException))

spec :: Spec
spec = sequential $ describe "stf" $ do
    modifyMaxSuccess (const 20) $ modifyMaxShrinks (const 0) $ it "stf" $ hedgehog $ do
        stmt <- normalise <$> forAll genStmt
        let prog = Program {methods=[], main=Just stmt}
        let stf = strongestTraceFormula prog
        let inst = head (ftcCondition prog stf)
        -- liftIO $ print $ pretty inst
        let smt = withDebug z3 False
        validity <- liftIO (try $ checkValidity smt inst :: IO (Either SomeException Validity))
        case validity of
          Left exception -> do
            liftIO $ print (pretty stmt)
            liftIO $ print stmt
            error $ show exception
          Right validity' -> validity' === Valid

genName :: (MonadGen m) => m Text
genName = Gen.element $ group $ pack ['a' .. 'z']

genInt :: (MonadGen m) => m Int
genInt = Gen.int $ Range.constant (1) 255

genArthExpr :: (MonadGen m) => m ArithmeticExpr
genArthExpr =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      Expr.Constant <$> genInt,
      Expr.AVar <$> genName
    ]
    [ -- recursive generators
      Gen.subterm genArthExpr Expr.Negation,
      Gen.subterm2 genArthExpr genArthExpr Expr.Plus,
      Gen.subterm2 genArthExpr genArthExpr Expr.Minus,
      Gen.subterm2 genArthExpr genArthExpr Expr.Times--,
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
    [ Gen.subterm2 genStmt genStmt Sequence--,
      -- do
      --   cond <- Condition <$> genBoolExpr
      --   Gen.subterm2 genStmt genStmt cond
    ]