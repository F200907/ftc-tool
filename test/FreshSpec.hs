{-# LANGUAGE OverloadedStrings #-}

module FreshSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Variable (fresh)

spec :: Spec
spec = describe "freshness" $ do
  it "freshness" $ hedgehog $ do
    idx <- forAll $ Gen.integral (Range.linear 0 100)
    let illegals = allIds idx
    let idx' = fresh illegals
    Set.member idx' illegals === False

allIds :: Int -> Set Text
allIds a = Set.fromList $ map (T.cons '$' . T.pack . show) [0..a]