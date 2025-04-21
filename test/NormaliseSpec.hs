module NormaliseSpec (spec) where

import Data.Trace.Normalise (Normalisable (..))
import Data.Trace.Program (Statement (..))
import SpecUtil
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, modifyMaxShrinks, modifyMaxSuccess, (===))
import Data.Trace.TraceLogic (TraceFormula(..))

spec :: Spec
spec = describe "normalise" $ modifyMaxSuccess (const 150) $ modifyMaxShrinks (const 100) $ do
  it "statements" $ hedgehog $ do
    stmt <- normalise <$> forAll genStmt
    normalisedStmt stmt === True

  it "trace formulas" $ hedgehog $ do
    tf <- normalise <$> forAll genTF
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
  _ -> normalisedTF t1 && normalisedTF t2
normalisedTF (Disjunction t1 t2) = normalisedTF t1 && normalisedTF t2
normalisedTF (Mu _ t) = normalisedTF t
normalisedTF _ = True
