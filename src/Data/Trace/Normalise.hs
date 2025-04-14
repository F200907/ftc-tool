{-# LANGUAGE InstanceSigs #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Data.Trace.Normalise (Normalisable (..)) where

-- import Data.Text ()

import Data.Trace.Program (Program (..), Statement (..), (#))
import Data.Trace.TraceLogic (TraceFormula (..), expandP)

class Normalisable a where
  -- |
  -- Normalises an inductive structure such that certain binary connectives are nested to the right.
  normalise :: a -> a

instance (Normalisable TraceFormula) where
  normalise :: TraceFormula -> TraceFormula
  normalise = normalise' . expandP
    where
      normalise' = distribute . chopAssoc

      chopAssoc :: TraceFormula -> TraceFormula
      chopAssoc tf@(StateFormula _) = tf
      chopAssoc tf@(BinaryRelation _) = tf
      chopAssoc tf@(RecursiveVariable _) = tf
      chopAssoc (Conjunction tf1 tf2) = Conjunction (chopAssoc tf1) (chopAssoc tf2)
      chopAssoc (Disjunction tf1 tf2) = Disjunction (chopAssoc tf1) (chopAssoc tf2)
      chopAssoc (Chop tf1 tf3) = case chopAssoc tf1 of
        Chop tf1' tf2' -> Chop tf1' (chopAssoc (Chop tf2' tf3))
        tf1' -> Chop tf1' (chopAssoc tf3)
      chopAssoc (Mu x tf') = Mu x (chopAssoc tf')

      distribute :: TraceFormula -> TraceFormula
      distribute (Chop (Disjunction tf1 tf2) tf3) = normalise' (Disjunction (Chop tf1 tf3) (Chop tf2 tf3))
      distribute (Chop (Conjunction tf1 tf2) tf3) = normalise' (Conjunction (Chop tf1 tf3) (Chop tf2 tf3))
      distribute (Chop tf1 tf2) = Chop tf1 (distribute tf2)
      distribute tf@(StateFormula _) = tf
      distribute tf@(BinaryRelation _) = tf
      distribute tf@(RecursiveVariable _) = tf
      distribute (Conjunction tf1 tf2) = Conjunction (distribute tf1) (distribute tf2)
      distribute (Disjunction tf1 tf2) = Disjunction (distribute tf1) (distribute tf2)
      distribute (Mu x tf) = Mu x (distribute tf)

instance (Normalisable Statement) where
  normalise :: Statement -> Statement
  normalise Skip = Skip
  -- normalise (Sequence (Condition b s1 s2) s) = normalise $ Condition b (s1 # s) (s2 # s)
  normalise s@(Assignment _ _) = s
  normalise (Sequence s1 s3) = case normalise s1 of
    Sequence s1' s2' -> Sequence s1' (normalise (Sequence s2' s3))
    s1' -> Sequence s1' (normalise s3)
  normalise (Condition b s1 s2) = Condition b (normalise s1) (normalise s2)
  normalise s@(Method _) = s

instance (Normalisable Program) where
  normalise :: Program -> Program
  normalise (Program xs m) =
    Program
      (map (\(m', c, s) -> (m', c, normalise s)) xs)
      ( do
          normalise <$> m
      )

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