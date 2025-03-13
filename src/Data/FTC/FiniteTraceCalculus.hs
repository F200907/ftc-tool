{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}
module Data.FTC.FiniteTraceCalculus (ftc) where

import Data.FTC.SMTFormula (SMTFormula (Not, Predicate), (&&&), (==>))
import Data.FTC.SMTPredicate (SMTPredicate (AssignmentPredicate, IdentityPredicate), predicate)
import Data.Set (Set)
import Data.Text (Text)
import Data.Trace.Program (Statement (Assignment, Condition, Sequence, Skip), (#))
import Data.Trace.TraceLogic (TraceFormula (Chop, StateFormula))
import Prelude hiding (id)

id :: Int -> SMTFormula
id i = Predicate (IdentityPredicate i)

ftc :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula
ftc i (Assignment x a) phi _ =
  Predicate (AssignmentPredicate i x a)
    ==> Predicate (predicate i phi)
ftc i Skip phi _ =
  id i
    ==> Predicate (predicate i phi)
ftc i (Condition b s1 s2) (Chop phi psi) x =
  id i
    ==> ( Predicate (predicate i phi)
            &&& (bPred ==> ftc (i + 1) s1 psi x)
            &&& (Not bPred ==> ftc (i + 1) s2 psi x)
        )
  where
    bPred = Predicate (predicate i (StateFormula b))
ftc i (Sequence (Assignment x a) s) (Chop phi psi) xs =
  Predicate (AssignmentPredicate i x a)
    ==> ( Predicate (predicate i phi)
            &&& ftc (i + 1) s psi xs
        )
ftc i (Sequence Skip s) (Chop phi psi) xs =
  id i
    ==> ( Predicate (predicate i phi)
            &&& ftc (i + 1) s psi xs
        )
ftc i (Sequence (Condition b s1 s2) s) (Chop phi psi) xs =
  id i
    ==> ( Predicate (predicate i phi)
            &&& (bPred ==> ftc (i + 1) (s1 # s) psi xs)
            &&& (Not bPred ==> ftc (i + 1) (s2 # s) psi xs)
        )
  where
    bPred = Predicate (predicate i (StateFormula b))
ftc _ _ _ _ = undefined