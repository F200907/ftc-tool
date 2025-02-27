{-# LANGUAGE OverloadedStrings #-}

module Data.FOL.SMTUtil (StateType (..), genState) where

import Data.Expression (VariableName)
import Data.List (delete, intersperse)
import Data.Text (Text)
import qualified Data.Text as Text

newtype StateType = StateType [VariableName]

genState :: StateType -> Text
genState (StateType xs) = declareDatatype <> "\n" <> idPred <> "\n" <> foldl (\acc x -> acc <> sbPred x <> "\n") "" xs
  where
    declareDatatype = "(declare-datatypes ((State 0)) (((mk-state " <> concatWithSpace declareDatatype' <> "))))"
    declareDatatype' = map (\x -> "(" <> x <> " Int)") xs
    idPred = "(define-fun id ((s1 State) (s2 State)) Bool (and " <> concatWithSpace (equivs xs) <> "))"
    sbPred x = "(define-fun sb_" <> x <> " ((s1 State) (s2 State) (val Int)) Bool (and " <> concatWithSpace ("(= (" <> x <> " s2) val)" : equivs (delete x xs)) <> "))"
    equivs = map (\x -> "(= (" <> x <> " s1) (" <> x <> " s2))")
    concatWithSpace = Text.concat . intersperse " "
