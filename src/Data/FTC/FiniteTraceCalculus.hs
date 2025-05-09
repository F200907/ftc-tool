
{-# HLINT ignore "Redundant IdentityPredicate" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Data.FTC.FiniteTraceCalculus (ftc, mc, constraints, depth, SideCondition, FTCProblem (..), fromFormula) where

import Data.Expression (BooleanExpr)
import Data.FTC.Contract (Contracts, lookupContract)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Trace.Normalise (Normalisable (..))
import Data.Trace.Program
import Data.Trace.TraceLogic (TraceFormula (..), unfold)
import qualified Debug.Trace as Trace
import SMT.Formula (SMTFormula (..), predicate, (&&&), (==>), (|||))
import SMT.Instance (SMTInstance (..), instanceOf)

data FTCProblem a = FTCProblem {dependencies :: [(a, FTCProblem a)], inst :: SMTInstance a} deriving (Show, Eq)

fromFormula :: SMTFormula a -> FTCProblem a
fromFormula f = FTCProblem [] (instanceOf [] f)

injectDep :: FTCProblem a -> a -> FTCProblem a -> FTCProblem a
injectDep (FTCProblem {dependencies, inst}) key val = FTCProblem ((key, val) : dependencies) inst

liftInst :: FTCProblem a -> (SMTInstance a -> SMTInstance a) -> FTCProblem a
liftInst (FTCProblem {dependencies, inst}) f = FTCProblem dependencies (f inst)

addCondition :: FTCProblem a -> SMTFormula a -> FTCProblem a
addCondition p f = liftInst p (\inst -> SMTInstance (f : conditions inst) (problem inst))

combine :: (SMTFormula a -> SMTFormula a -> SMTFormula a) -> FTCProblem a -> FTCProblem a -> FTCProblem a
combine op a b = FTCProblem (dependencies a ++ dependencies b) (SMTInstance [] (op (problem (inst a)) (problem (inst b))))

type SideCondition = (Text, TraceFormula)

trace a b = Trace.trace a b
-- trace _ b = b

depth :: Statement -> Int
depth Skip = 1
depth (Assignment _ _) = 1
depth (Condition _ s1 s2) = max (depth s1) (depth s2) + 1
depth (Sequence s1 s2) = depth s1 + depth s2
depth (Method _) = 1

ftc :: Program -> TraceFormula -> FTCProblem SideCondition
ftc p tf0 = case main p of
  Just s ->
    let f = ftc' 1 s tf0 Set.empty Top
     in addCondition f (constraints cs 1 s)
  _ -> FTCProblem [] (instanceOf [] Bot)
  where
    cs = contracts p
    pre' m = case lookupContract m cs of
      Just (pre, _) -> pre
      Nothing -> error "FIXME: no contract found for a procedure"

    ftc' :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula SideCondition -> FTCProblem SideCondition
    ftc' i s mu@(Mu _ _) xs eta = ftc' i s (unfold mu) xs eta
    ftc' i s (Chop mu@(Mu _ _) tf2) xs eta = ftc' i s (normalise (Chop (unfold mu) tf2)) xs eta
    ftc' i s (Disjunction tf1 tf2) xs eta = combine Or (ftc' i s tf1 xs eta) (ftc' i s tf2 xs eta)
    ftc' i s (Conjunction tf1 tf2) xs eta = combine And (ftc' i s tf1 xs eta) (ftc' i s tf2 xs eta)
    --
    ftc' i (Assignment _ _) phi _ eta = fromFormula $ predicate i phi &&& eta
    ftc' i Skip phi _ eta = trace (show phi) $ fromFormula $ predicate i phi &&& eta
    ftc' i (Condition b s1 s2) (Chop tf1 tf2) xs eta =
      let f1 = ftc' (i + 1) s1 tf2 xs (predicate i tf1 &&& eta)
          f2 = ftc' (i + 1) s2 tf2 xs (predicate i tf1 &&& eta)
          deps1 = dependencies f1
          deps2 = dependencies f2
          prob1 = problem (inst f1)
          prob2 = problem (inst f2)
       in FTCProblem (deps1 ++ deps2) (instanceOf [] ((StatePredicate i b ==> prob1) &&& (Not (StatePredicate i b) ==> prob2)))
    ftc' i (Method m) (Chop phi mu@(Mu x _)) xs eta
      | x `Set.member` xs = fromFormula tf
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              mu' = unfold mu
              f' = addCondition (addCondition (ftc' 1 sm mu' (Set.insert x xs) Top) (constraints cs 1 sm)) (StatePredicate 1 (pre' m))
           in injectDep (fromFormula $ ConstantPred (m, mu) &&& tf) (m, mu) f'
      where
        tf = StatePredicate (i + 1) (pre' m) &&& predicate i phi &&& eta
    --
    ftc' i (Sequence (Assignment _ _) s2) (Chop tf1 tf2) xs eta = ftc' (i + 1) s2 tf2 xs (predicate i tf1 &&& eta)
    ftc' i (Sequence Skip s2) (Chop tf1 tf2) xs eta = ftc' (i + 1) s2 tf2 xs (predicate i tf1 &&& eta)
    ftc' i (Sequence (Method m) s2) (Chop tf1 (Chop mu@(Mu x _) tf2)) xs eta
      | x `Set.member` xs = ftc' (i + 2) s2 tf2 xs (StatePredicate (i + 1) (pre' m) &&& predicate i tf1 &&& eta)
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              mu' = unfold mu
              f' = addCondition (addCondition (ftc' 1 sm mu' (Set.insert x xs) Top) (constraints cs 1 sm)) (StatePredicate 1 (pre' m))
           in injectDep (ftc' (i + 2) s2 tf2 xs (ConstantPred (m, mu) &&& StatePredicate (i + 1) (pre' m) &&& predicate i tf1 &&& eta)) (m, mu) f'
    ftc' _ _ _ _ _ = fromFormula Bot

ftc'' :: Program -> Int -> Statement -> TraceFormula -> Set Text -> SMTFormula SideCondition -> [SMTInstance SideCondition]
ftc'' p i0 s0 tf0 xs0 eta0 =
  let (problem, sideconds) = ftc' i0 s0 tf0 xs0 eta0
   in {-Trace.trace (show [show p, show tf0, "\n"]) $-} instanceOf [constraints cs i0 s0] problem : sideconds
  where
    cs = contracts p

    pre' m = case lookupContract m cs of
      Just (pre, _) -> pre
      Nothing -> error "FIXME: no contract found for a procedure"

    -- m' :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula -> (SMTFormula, [SMTInstance])
    -- m' i (Method m)
    -- m' _ _ _ _ _ = undefined

    ftc' :: Int -> Statement -> TraceFormula -> Set Text -> SMTFormula SideCondition -> (SMTFormula SideCondition, [SMTInstance SideCondition])
    ftc' i s mu@(Mu _ _) xs eta = ftc' i s (unfold mu) xs eta
    ftc' i s (Chop mu@(Mu _ _) tf2) xs eta = trace "Unwind" $ ftc' i s (normalise (Chop (unfold mu) tf2)) xs eta
    ftc' i s tf@(Disjunction phi1 phi2) xs eta =
      let (f1, a1) = ftc' i s phi1 xs eta
          (f2, a2) = ftc' i s phi2 xs eta
       in trace ("Dis: " ++ show [show i, show s, show tf, show xs, show eta]) (f1 ||| f2, a1 ++ a2)
    ftc' i s tf@(Conjunction phi1 phi2) xs eta =
      let (f1, a1) = ftc' i s phi1 xs eta
          (f2, a2) = ftc' i s phi2 xs eta
       in trace ("Con: " ++ show [show i, show s, show tf, show xs, show eta]) (f1 &&& f2, a1 ++ a2)
    -- ftc' _ (Assignment _ _) (Chop _ _) _ _ = (Bot, [])
    -- ftc' _ Skip (Chop _ _) _ _ = (Bot, [])
    ftc' i (Assignment _ _) phi _ eta = (predicate i phi &&& eta, [])
    ftc' i Skip phi _ eta = (predicate i phi &&& eta, [])
    ftc' i s@(Condition b s1 s2) tf@(Chop phi phi') xs eta =
      let (f1, a1) = ftc' (i + 1) s1 phi' xs (predicate i phi &&& eta)
          (f2, a2) = ftc' (i + 1) s2 phi' xs (predicate i phi &&& eta)
       in trace ("Cond: " ++ show [show i, show s, show tf, show phi', show xs, show eta]) $ ((StatePredicate i b ==> f1) &&& (Not (StatePredicate i b) ==> f2), a1 ++ a2)
    ftc' i (Method m) (Chop phi mu@(Mu x _)) xs eta
      | x `Set.member` xs = (StatePredicate i (pre' m) &&& predicate i phi &&& eta, [])
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              mu' = unfold mu
              (f', a') = ftc' 1 sm mu' (Set.insert x xs) Top
              inst' = instanceOf [constraints cs 1 sm, StatePredicate 1 (pre' m)] f'
           in (StatePredicate i (pre' m) &&& predicate i phi &&& eta, inst' : a')
    ftc' i (Sequence (Assignment _ _) s') (Chop phi phi') xs eta = ftc' (i + 1) s' phi' xs (predicate i phi &&& eta)
    ftc' i s@(Sequence Skip s') tf@(Chop phi phi') xs eta = trace ("Skip: " ++ show [show i, show s, show tf, show phi', show xs, show eta]) $ ftc' (i + 1) s' phi' xs (predicate i phi &&& eta)
    -- ftc' i (Sequence (Condition b s1 s2) s') tf xs eta = ftc' i (Condition b (normalise (s1 # s')) (normalise (s2 # s'))) tf xs eta
    ftc' i (Sequence (Method m) s') (Chop phi (Chop mu@(Mu x _) phi')) xs eta
      | x `Set.member` xs = ftc' (i + 1) s' phi' xs (StatePredicate i (pre' m) &&& predicate i phi &&& eta)
      | otherwise =
          let sm = fromMaybe (error "procedure not defined") (methodBody p m)
              mu' = unfold mu
              (f', a') = ftc' 1 sm mu' (Set.insert x xs) Top
              (f'', a'') = ftc' (i + 1) s' phi' xs (StatePredicate i (pre' m) &&& predicate i phi &&& eta)
              inst' = instanceOf [constraints cs 1 sm, StatePredicate 1 (pre' m)] f'
           in (f'', inst' : a' ++ a'')
    -- ftc' i (Sequence (Sequence s1 s2) s3) phi xs eta = ftc' i (s1 # (s2 # s3)) phi xs eta
    -- ftc' i s (StateFormula sf) xs eta = (StatePredicate i sf, [])
    -- ftc' i s b@(BinaryRelation _) xs eta = (predicate (i - 1) b, [])
    ftc' i s tf xs eta = trace ("Empty: " ++ show [show i, show s, show tf, show xs, show eta]) (Bot, [])

-- ftc' _ _ _ _ _ = (error $ show Bot, [])

mc :: Contracts -> Int -> Statement -> BooleanExpr -> SMTFormula a
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
constraints :: Contracts -> Int -> Statement -> SMTFormula a -- maybe change to [SMTFormula] for several conditions instead of big conjunction
constraints cs = constraints'
  where
    condition i b = predicate i (StateFormula b)
    contractImpl i m =
      let (pre, post) = fromMaybe (error "FIXME: no contract found for a procedure") (lookupContract m cs)
       in (StatePredicate (i + 1) pre ==> BinaryPredicate (i + 1) (i + 2) post)

    constraints' i (Assignment x a) = AssignmentPredicate i x a
    constraints' i Skip = IdentityPredicate i
    constraints' i (Condition b s1 s2) = IdentityPredicate i &&& (condition i b ==> constraints' (i + 1) s1) &&& (Not (condition i b) ==> constraints' (i + 1) s2)
    constraints' i (Method m) = contractImpl i m &&& IdentityPredicate i
    constraints' i (Sequence (Assignment x a) s') = AssignmentPredicate i x a &&& constraints' (i + 1) s'
    constraints' i (Sequence Skip s') = IdentityPredicate i &&& constraints' (i + 1) s'
    constraints' i (Sequence (Condition b s1 s2) s') = IdentityPredicate i &&& (condition i b ==> constraints' (i + 1) (s1 # s')) &&& (Not (condition i b) ==> constraints' (i + 1) (s2 # s'))
    constraints' i (Sequence (Method m) s') = contractImpl i m &&& IdentityPredicate i &&& constraints' (i + 2) s'
    constraints' i (Sequence (Sequence s1 s2) s3) = constraints' i (s1 # (s2 # s3))

-- recursive definition below
-- constraints' i (Sequence s@(Assignment _ _) s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence s@Skip s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence (Condition b s1 s2) s') = constraints' i (Condition b (s1 # s') (s2 # s'))
-- constraints' i (Sequence s@(Method _) s') = constraints' i s &&& constraints' (i + 1) s'
-- constraints' i (Sequence (Sequence s1 s2) s3) = constraints' i (s1 # (s2 # s3))