{-# LANGUAGE InstanceSigs #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Data.Trace.Normalise (Normalisable (..)) where

-- import Data.Text ()

import Data.Trace.Program (Program (..), Statement (..))
import Data.Trace.TraceLogic (TraceFormula (..))
import Debug.Trace (trace)

class Normalisable a where
  -- |
  -- Normalises an inductive structure such that certain binary connectives are nested to the right.
  normalise :: a -> a

instance (Normalisable TraceFormula) where
  normalise :: TraceFormula -> TraceFormula
  normalise x = let y = (distribute . assoc) x in
    if x == y then x else normalise y
    where
      assoc :: TraceFormula -> TraceFormula
      assoc tf@(StateFormula _) = tf
      assoc tf@(BinaryRelation _) = tf
      assoc tf@(RecursiveVariable _) = tf
      assoc (Conjunction tf1 tf2) = Conjunction (assoc tf1) (assoc tf2)
      assoc (Disjunction tf1 tf2) = Disjunction (assoc tf1) (assoc tf2)
      assoc (Chop tf1 tf3) = case assoc tf1 of
        Chop tf1' tf2' -> Chop tf1' (assoc (Chop tf2' tf3))
        tf1' -> Chop tf1' (assoc tf3)
      assoc (Mu x tf') = Mu x (assoc tf')

      distribute :: TraceFormula -> TraceFormula
      distribute (Chop (Disjunction tf1 tf2) tf3) = Disjunction (Chop tf1 tf3) (Chop tf2 tf3)
      distribute (Chop (Conjunction tf1 tf2) tf3) = Conjunction (Chop tf1 tf3) (Chop tf2 tf3)
      distribute (Chop tf1 (Disjunction tf2 tf3)) = Disjunction (Chop tf1 tf2) (Chop tf1 tf3)
      distribute (Chop tf1 (Conjunction tf2 tf3)) = Conjunction (Chop tf1 tf2) (Chop tf1 tf3)
      distribute (Chop tf1 tf2) = Chop (distribute tf1) (distribute tf2)
      distribute tf@(StateFormula _) = tf
      distribute tf@(BinaryRelation _) = tf
      distribute tf@(RecursiveVariable _) = tf
      distribute (Conjunction tf1 tf2) = Conjunction (distribute tf1) (distribute tf2)
      distribute (Disjunction tf1 tf2) = Disjunction (distribute tf1) (distribute tf2)
      distribute (Mu x tf) = Mu x (distribute tf)

instance (Normalisable Statement) where
  normalise :: Statement -> Statement
  normalise = distribute . assoc
    where
    assoc :: Statement -> Statement
    assoc Skip = Skip
    assoc s@(Assignment {}) = s
    assoc s@(Method {}) = s
    assoc (Condition b s1 s2) = Condition b (assoc s1) (assoc s2)
    assoc (Sequence s s') = case assoc s of
      Sequence s1 s2 -> Sequence s1 (assoc (Sequence s2 s'))
      _ -> Sequence s (assoc s')

    distribute :: Statement -> Statement
    distribute Skip = Skip
    distribute s@(Assignment {}) = s
    distribute s@(Method {}) = s
    distribute (Condition b s1 s2) = Condition b (distribute s1) (distribute s2)
    distribute (Sequence s s') = case s of
      Condition b s1 s2 -> Condition b (normalise (Sequence s1 s')) (normalise (Sequence s2 s'))
      _ -> Sequence (distribute s) (distribute s')

instance (Normalisable Program) where
  normalise :: Program -> Program
  normalise (Program xs m) =
    Program
      (map (\(m', c, s) -> (m', c, normalise s)) xs)
      (normalise <$> m)

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