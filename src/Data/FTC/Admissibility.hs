module Data.FTC.Admissibility (adm) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Trace.Program (Program, Statement (Assignment, Condition, Method, Sequence, Skip), methodBody)
import Data.Trace.TraceLogic (TraceFormula (Chop, Mu, RecursiveVariable))

adm :: Program -> Statement -> TraceFormula -> Set (Text, Text) -> Bool
adm prog = adm'
  where
    adm' Skip _ _ = True
    adm' (Assignment _ _) _ _ = True
    adm' (Sequence s1 s2) phi x = case phi of
      Chop t1 t2 -> adm' s1 t1 x && adm' s2 t2 x
      _ -> False
    adm' (Condition _ s1 s2) phi x = case phi of
      Chop _ t2 -> adm' s1 t2 x && adm' s2 t2 x
      _ -> False
    adm' (Method m) (RecursiveVariable xm) x = (m, xm) `Set.member` x
    adm' (Method m) (Mu xm phi) x
      | Set.null $ Set.filter (\(y, _) -> m == y) x = case methodBody prog m of
          Just s -> adm' s phi (Set.insert (m, xm) x)
          _ -> False
      | otherwise = False
    adm' _ _ _ = False