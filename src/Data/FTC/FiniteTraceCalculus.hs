{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}
module Data.FTC.FiniteTraceCalculus (ftc, mc) where

import Data.Expression (ArithmeticExpr, BooleanExpr, VariableName)
import Data.FTC.Contract (Contracts, lookupContract)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Trace.Program
import Data.Trace.TraceLogic (TraceFormula (Chop, StateFormula))
import SMT.SMTFormula (SMTFormula (Not, Predicate), (&&&), (==>))
import SMT.SMTPredicate (SMTPredicate (AssignmentPredicate, IdentityPredicate, StatePredicate), predicate)
import Prelude hiding (id)

id :: Int -> SMTFormula
id i = Predicate (IdentityPredicate i)

sb :: Int -> VariableName -> ArithmeticExpr -> SMTFormula
sb i x a = Predicate $ AssignmentPredicate i x a

ftc :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula
-- x := a
ftc i (Assignment x a) phi _ =
  sb i x a
    ==> Predicate (predicate i phi)
-- skip
ftc i Skip phi _ =
  id i
    ==> Predicate (predicate i phi)
-- if b then s1 else s2
ftc i (Condition b s1 s2) (Chop phi psi) x =
  id i
    ==> ( Predicate (predicate i phi)
            &&& (bPred ==> ftc (i + 1) s1 psi x)
            &&& (Not bPred ==> ftc (i + 1) s2 psi x)
        )
  where
    bPred = Predicate (predicate i (StateFormula b))
-- x := a; s
ftc i (Sequence (Assignment x a) s) (Chop phi psi) xs =
  sb i x a
    ==> ( Predicate (predicate i phi)
            &&& ftc (i + 1) s psi xs
        )
-- skip; s
ftc i (Sequence Skip s) (Chop phi psi) xs =
  id i
    ==> ( Predicate (predicate i phi)
            &&& ftc (i + 1) s psi xs
        )
-- (if b then s1 else s2); s
ftc i (Sequence (Condition b s1 s2) s) (Chop phi psi) xs =
  id i
    ==> ( Predicate (predicate i phi)
            &&& (bPred ==> ftc (i + 1) (s1 # s) psi xs)
            &&& (Not bPred ==> ftc (i + 1) (s2 # s) psi xs)
        )
  where
    bPred = Predicate (predicate i (StateFormula b))
-- m()
-- ftc i (Method m) (Chop phi (Mu x psi)) xs = undefined
ftc _ _ _ _ = undefined

mc :: Contracts -> Int -> Statement -> BooleanExpr -> SMTFormula
mc _ i (Assignment x a) phi = sb i x a ==> Predicate (StatePredicate (i + 1) phi)
mc _ i Skip phi = id i ==> Predicate (StatePredicate (i + 1) phi)
-- if b then s1 else s2
mc cs i (Condition b s1 s2) phi =
  id i
    ==> (bPred ==> mc cs (i + 1) s1 phi)
    &&& (Not bPred ==> mc cs (i + 1) s2 phi)
  where
    bPred = Predicate (predicate i (StateFormula b))
mc cs i (Method m) phi = (Predicate (StatePredicate i pre) &&& Predicate (StatePredicate (i + 1) post)) ==> Predicate (StatePredicate (i + 1) phi)
  where
    (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
mc cs i (Sequence (Assignment x a) s) phi = sb i x a ==> mc cs (i + 1) s phi
mc cs i (Sequence Skip s) phi = id i ==> mc cs (i + 1) s phi
mc cs i (Sequence (Condition b s1 s2) s) phi =
  id i
    ==> (bPred ==> mc cs (i + 1) (s1 # s) phi)
    &&& (Not bPred ==> mc cs (i + 1) (s2 # s) phi)
  where
    bPred = Predicate (predicate i (StateFormula b))
mc cs i (Sequence (Method m) s) phi = (Predicate (StatePredicate i pre) &&& Predicate (StatePredicate (i + 1) post)) ==> mc cs (i + 1) s phi
  where
    (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
mc cs i (Sequence (Sequence s1 s2) s3) phi = mc cs i (s1 # (s2 # s3)) phi