{-# LANGUAGE InstanceSigs #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Data.Trace.Normalise (Normalisable (..)) where

-- import Data.Text ()

import Data.Trace.Program (Statement (..))
import Data.Trace.TraceLogic (TraceFormula (..))

class Normalisable a where
  -- |
  -- Normalises an inductive structure such that certain binary connectives are nested to the right.
  normalise :: a -> a

instance (Normalisable TraceFormula) where
  normalise :: TraceFormula -> TraceFormula
  normalise tf@(StateFormula _) = tf
  normalise tf@(BinaryRelation _) = tf
  normalise tf@(RecursiveVariable _) = tf
  normalise (Conjunction tf1 tf2) = Conjunction (normalise tf1) (normalise tf2)
  normalise (Disjunction tf1 tf2) = Disjunction (normalise tf1) (normalise tf2)
  normalise (Chop tf1 tf3) = case normalise tf1 of
    Chop tf1' tf2' -> Chop tf1' (normalise (Chop tf2' tf3))
    tf1' -> Chop tf1' (normalise tf3)
  normalise (Mu x tf') = Mu x (normalise tf')

instance (Normalisable Statement) where
  normalise :: Statement -> Statement
  normalise Skip = Skip
  normalise s@(Assignment _ _) = s
  normalise (Sequence s1 s3) = case normalise s1 of
    Sequence s1' s2' -> Sequence s1' (normalise (Sequence s2' s3))
    s1' -> Sequence s1' (normalise s3)
  normalise (Condition b s1 s2) = Condition b (normalise s1) (normalise s2)
  normalise s@(Method _) = s

-- normaliseCheck :: TraceFormula -> Bool
-- normaliseCheck (Conjunction tf1 tf2) = normaliseCheck tf1 && normaliseCheck tf2
-- normaliseCheck (Disjunction tf1 tf2) = normaliseCheck tf1 && normaliseCheck tf2
-- normaliseCheck (Mu _ tf') = normaliseCheck tf'
-- normaliseCheck (Chop tf1 tf2) = case tf1 of
--   Chop _ _ -> False
--   _ -> normaliseCheck tf1 && normaliseCheck tf2
-- normaliseCheck _ = True

-- allChops 0 = [RecursiveVariable "x"]
-- allChops n =
--   [ Chop left right
--     | i <- [0 .. n - 1],
--       left <- allChops i,
--       right <- allChops (n - 1 - i)
--   ]

-- tts n = let tfs = allChops n
--             tfs' = map (normaliseCheck . normalise) tfs
--             ls = zip tfs tfs' in
--                 head $ filter (\(x, y) -> not y) ls