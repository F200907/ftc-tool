{-# LANGUAGE OverloadedStrings #-}

module STFSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Trace.Normalise (Normalisable (normalise))
import Data.Trace.Program (Program (..))
import Data.Trace.TraceLogic (strongestTraceFormula, expandP)
import Prettyprinter (Pretty (pretty))
import SMT.Solver (Validity (Valid), checkValidity, ftcCondition, withDebug, z3)
import Test.Hspec (Spec, describe, it, sequential)
import Test.Hspec.Hedgehog (forAll, hedgehog, modifyMaxShrinks, modifyMaxSuccess, (===))
import SpecUtil

spec :: Spec
spec = sequential $ describe "stf" $ do
  modifyMaxSuccess (const 50) $ modifyMaxShrinks (const 10) $ it "stf" $ hedgehog $ do
    stmt <- forAll genStmt
    let prog = Program {methods = [], main = Just stmt}
    let stf = strongestTraceFormula prog
    let inst = head (ftcCondition (normalise prog) (normalise stf))
    -- liftIO $ print $ pretty inst
    let smt = withDebug z3 False
    validity <- liftIO (try $ checkValidity smt inst :: IO (Either SomeException Validity))
    case validity of
      Left exception -> do
        liftIO $ print (pretty stmt)
        liftIO $ print stmt
        error $ show exception
      Right validity' -> validity' === Valid

