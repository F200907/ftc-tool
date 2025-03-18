{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FTC.ProofObligation (ProofObligation(..), StateInfo(..)) where
import Data.Text (Text)
import Data.FOL.SMTUtil (SMTify (smtify))
import qualified Data.Text as Text
import Data.List (intersperse, delete)
import Data.FTC.SMTFormula (SMTFormula)
newtype StateInfo = StateInfo [Text]

data ProofObligation = ProofObligation {stateInfo :: StateInfo, formula :: [SMTFormula], sideConditions :: [SMTFormula]}

instance (SMTify StateInfo) where
    smtify :: StateInfo -> Text
    smtify (StateInfo xs) = declareDatatype <> "\n" <> idPred <> "\n" <> foldl (\acc x -> acc <> sbPred x <> "\n") "" xs
        where
            declareDatatype = "(declare-datatypes ((State 0)) (((mk-state " <> concatWithSpace declareDatatype' <> "))))"
            declareDatatype' = map (\x -> "(" <> x <> " Int)") xs
            idPred = "(define-fun id ((s1 State) (s2 State)) Bool (and " <> concatWithSpace (equivs xs) <> "))"
            sbPred x = "(define-fun sb_" <> x <> " ((s1 State) (s2 State) (val Int)) Bool (and " <> concatWithSpace ("(= (" <> x <> " s2) val)" : equivs (delete x xs)) <> "))"
            equivs = map (\x -> "(= (" <> x <> " s1) (" <> x <> " s2))")
            concatWithSpace = Text.concat . intersperse " "

-- f :: StateT ProofObligation
-- f = undefined