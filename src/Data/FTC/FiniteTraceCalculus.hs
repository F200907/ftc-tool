{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant IdentityPredicate" #-}
module Data.FTC.FiniteTraceCalculus (ftc, mc, constraints) where

import Data.Expression (BooleanExpr)
import Data.FTC.Contract (Contracts, lookupContract)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Trace.Program
import Data.Trace.TraceLogic (TraceFormula (Chop, Mu, StateFormula), unfold)
import SMT.Formula (SMTFormula (..), predicate, (&&&), (==>))
import SMT.Instance (SMTInstance (SMTInstance), instanceOf)

-- ftc :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula
-- -- x := a
-- ftc i (Assignment x a) phi _ =
--   AssignmentPredicate i x a
--     ==> predicate i phi
-- -- skip
-- ftc i Skip phi _ =
--   IdentityPredicate i
--     ==> predicate i phi
-- -- if b then s1 else s2
-- ftc i (Condition b s1 s2) (Chop phi psi) x =
--   IdentityPredicate i
--     ==> ( predicate i phi
--             &&& (bPred ==> ftc (i + 1) s1 psi x)
--             &&& (Not bPred ==> ftc (i + 1) s2 psi x)
--         )
--   where
--     bPred = predicate i (StateFormula b)
-- -- x := a; s
-- ftc i (Sequence (Assignment x a) s) (Chop phi psi) xs =
--   AssignmentPredicate i x a
--     ==> ( predicate i phi
--             &&& ftc (i + 1) s psi xs
--         )
-- -- skip; s
-- ftc i (Sequence Skip s) (Chop phi psi) xs =
--   IdentityPredicate i
--     ==> ( predicate i phi
--             &&& ftc (i + 1) s psi xs
--         )
-- -- (if b then s1 else s2); s
-- ftc i (Sequence (Condition b s1 s2) s) (Chop phi psi) xs =
--   IdentityPredicate i
--     ==> ( predicate i phi
--             &&& (bPred ==> ftc (i + 1) (s1 # s) psi xs)
--             &&& (Not bPred ==> ftc (i + 1) (s2 # s) psi xs)
--         )
--   where
--     bPred = predicate i (StateFormula b)
-- -- m()
-- -- ftc i (Method m) (Chop phi (Mu x psi)) xs = undefined
-- ftc _ _ _ _ = undefined

-- goals :: Statement -> TraceFormula -> [(Statement, TraceFormula)]
-- goals (Assignment _ _)

ftc :: Program -> Int -> Statement -> TraceFormula -> Set Text -> SMTFormula -> [SMTInstance]
ftc p i' s tf xs' eta' =
  let (problem, sideconds) = ftc' i' s tf xs' eta'
   in instanceOf [constraints cs i' s] problem : sideconds
  where
    cs = contracts p

    pre' m = case lookupContract m cs of
      Just (pre, _) -> pre
      Nothing -> error "FIXME: no contract found for a procedure"

    ftc' :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula -> (SMTFormula, [SMTInstance])
    ftc' i (Assignment _ _) phi _ eta = (predicate i phi &&& eta, [])
    ftc' i Skip phi _ eta = (predicate i phi &&& eta, [])
    ftc' i (Condition b s1 s2) (Chop phi phi') xs eta =
      let (f1, a1) = ftc' (i + 1) s1 phi' xs (predicate i phi &&& eta)
          (f2, a2) = ftc' (i + 1) s2 phi' xs (predicate i phi &&& eta)
       in ((StatePredicate i b ==> f1) &&& (Not (StatePredicate i b) ==> f2), a1 ++ a2)
    ftc' i (Method m) (Chop phi mu@(Mu x _)) xs eta
      | x `Set.member` xs = (StatePredicate i (pre' m) &&& predicate i phi &&& eta, [])
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              xs' = Set.insert x xs
              mu' = unfold mu
              (f', a') = ftc' 1 sm mu' xs' Top
              inst' = instanceOf [constraints cs 1 sm, StatePredicate 1 (pre' m)] f'
           in (StatePredicate i (pre' m) &&& predicate i phi &&& eta, inst' : a')
    ftc' i (Sequence (Assignment _ _) s') (Chop phi phi') xs eta = ftc' (i + 1) s' phi' xs (predicate i phi &&& eta)
    ftc' i (Sequence Skip s') (Chop phi phi') xs eta = ftc' (i + 1) s' phi' xs (predicate i phi &&& eta)
    ftc' i (Sequence (Condition b s1 s2) s') tf@(Chop _ _) xs eta = ftc' i (Condition b (s1 # s') (s2 # s')) tf xs eta
    ftc' i (Sequence (Method m) s') (Chop phi (Chop mu@(Mu x _) phi')) xs eta
      | x `Set.member` xs = ftc' (i + 1) s' phi' xs (StatePredicate i (pre' m) &&& predicate i phi &&& eta)
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              xs' = Set.insert x xs
              mu' = unfold mu
              (f', a') = ftc' 1 sm mu' xs' Top
              (f'', a'') = ftc' (i + 1) s' phi' xs (StatePredicate i (pre' m) &&& predicate i phi &&& eta)
              inst' = instanceOf [constraints cs 1 sm, StatePredicate 1 (pre' m)] f'
           in (f'', inst' : a' ++ a'')
    ftc' i (Sequence (Sequence s1 s2) s3) phi xs eta = ftc' i (s1 # (s2 # s3)) phi xs eta
    ftc' _ _ _ _ _ = (Bot, [])

mc :: Contracts -> Int -> Statement -> BooleanExpr -> SMTFormula
mc cs' i' s' phi' = mc' cs' i' s' phi'
  where
    postM = BinaryPredicate 1 (i' + 1) phi'
    mc' _ i (Assignment x a) _ = AssignmentPredicate i x a ==> postM
    mc' _ i Skip _ = IdentityPredicate i ==> postM
    -- if b then s1 else s2
    mc' cs i (Condition b s1 s2) phi =
      IdentityPredicate i
        ==> (bPred ==> mc cs (i + 1) s1 phi)
        &&& (Not bPred ==> mc cs (i + 1) s2 phi)
      where
        bPred = predicate i (StateFormula b)
    mc' cs i (Method m) _ = (StatePredicate i pre ==> BinaryPredicate i (i + 1) post) ==> postM
      where
        (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
    mc' cs i (Sequence (Assignment x a) s) phi = AssignmentPredicate i x a ==> mc cs (i + 1) s phi
    mc' cs i (Sequence Skip s) phi = IdentityPredicate i ==> mc cs (i + 1) s phi
    mc' cs i (Sequence (Condition b s1 s2) s) phi =
      IdentityPredicate i
        ==> (bPred ==> mc cs (i + 1) (s1 # s) phi)
        &&& (Not bPred ==> mc cs (i + 1) (s2 # s) phi)
      where
        bPred = predicate i (StateFormula b)
    mc' cs i (Sequence (Method m) s) phi = (StatePredicate i pre ==> BinaryPredicate i (i + 1) post) ==> mc cs (i + 1) s phi
      where
        (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
    mc' cs i (Sequence (Sequence s1 s2) s3) phi = mc cs i (s1 # (s2 # s3)) phi

-- |
--  Computes a conjunction of constraints that every trace of the program is satisfying
constraints :: Contracts -> Int -> Statement -> SMTFormula -- maybe change to [SMTFormula] for several conditions instead of big conjunction
constraints cs = constraints'
  where
    condition i b = predicate i (StateFormula b)
    contractImpl i m =
      let (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
       in (StatePredicate i pre ==> BinaryPredicate i (i + 1) post)

    constraints' i (Assignment x a) = AssignmentPredicate i x a
    constraints' i Skip = IdentityPredicate i
    constraints' i (Condition b s1 s2) = IdentityPredicate i &&& (condition i b ==> constraints' (i + 1) s1) &&& (Not (condition i b) ==> constraints' (i + 1) s2)
    constraints' i (Method m) = contractImpl i m
    constraints' i (Sequence (Assignment x a) s') = AssignmentPredicate i x a &&& constraints' (i + 1) s'
    constraints' i (Sequence Skip s') = IdentityPredicate i &&& constraints' (i + 1) s'
    constraints' i (Sequence (Condition b s1 s2) s') = IdentityPredicate i &&& (condition i b ==> constraints' (i + 1) (s1 # s')) &&& (Not (condition i b) ==> constraints' (i + 1) (s2 # s'))
    constraints' i (Sequence (Method m) s') = contractImpl i m &&& constraints' (i + 1) s'
    constraints' i (Sequence (Sequence s1 s2) s3) = constraints' i (s1 # (s2 # s3))

-- recursive definition below
-- constraints' i (Sequence s@(Assignment _ _) s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence s@Skip s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence (Condition b s1 s2) s') = constraints' i (Condition b (s1 # s') (s2 # s'))
-- constraints' i (Sequence s@(Method _) s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence (Sequence s1 s2) s3) = constraints' i (s1 # (s2 # s3))