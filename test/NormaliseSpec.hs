{-# LANGUAGE OverloadedStrings #-}

module NormaliseSpec (spec) where

import Data.Expression (BooleanExpr (BTrue))
import Data.Trace.Normalise (Normalisable (..))
import Data.Trace.Program (Statement (..))
import Data.Trace.TraceLogic (BinaryRelation (Id), TraceFormula (..), expandP)
import SpecUtil
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, modifyMaxShrinks, modifyMaxSuccess, (===))

spec :: Spec
spec = describe "normalise" $ modifyMaxSuccess (const 1000) $ modifyMaxShrinks (const 100) $ do
  it "statements" $ hedgehog $ do
    stmt <- normalise <$> forAll genStmt
    normalisedStmt stmt === True

  it "trace formulas" $ hedgehog $ do
    tf <- normalise <$> forAll genTF
    normalisedTF tf === True

  modifyMaxSuccess (const 1) $ it "cherry-pick" $ hedgehog $ do
    let true = StateFormula BTrue
    let muY = Mu "Y" (BinaryRelation Id)
    let tf = normalise $ Chop (Mu "X" (Chop (expandP true) muY)) (BinaryRelation Id)
    tf === Chop (Mu "X" (Disjunction (Chop true muY) (Chop true (Chop (Mu "$0" (Disjunction true (Chop true (RecursiveVariable "$0")))) muY)))) (BinaryRelation Id)
    normalisedTF tf === True

normalisedStmt :: Statement -> Bool
normalisedStmt Skip = True
normalisedStmt (Assignment _ _) = True
normalisedStmt (Sequence (Condition {}) _) = False
normalisedStmt (Condition _ s1 s2) = normalisedStmt s1 && normalisedStmt s2
normalisedStmt (Sequence s1 s2) = case s1 of
  Sequence _ _ -> False
  _ -> normalisedStmt s1 && normalisedStmt s2
normalisedStmt (Method _) = True

normalisedTF :: TraceFormula -> Bool
normalisedTF (Conjunction t1 t2) = normalisedTF t1 && normalisedTF t2
normalisedTF (Chop t1 t2) = case t1 of
  Chop _ _ -> False
  Disjunction _ _ -> False
  Conjunction _ _ -> False
  _ -> case t2 of
    Disjunction _ _ -> False
    Conjunction _ _ -> False
    _ -> normalisedTF t1 && normalisedTF t2
normalisedTF (Disjunction t1 t2) = normalisedTF t1 && normalisedTF t2
normalisedTF (Mu _ t) = normalisedTF t
normalisedTF _ = True
