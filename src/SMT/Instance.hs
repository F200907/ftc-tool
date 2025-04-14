{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SMT.Instance (SMTInstance (..), invalidInstance, instanceOf) where

import Data.Set (Set)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (..), line, vsep)
import SMT.Formula (SMTFormula (..))
import qualified Data.Set as Set
import Data.Variable

data SMTInstance = SMTInstance {conditions :: [SMTFormula], problem :: SMTFormula} deriving (Show)

instance (Pretty SMTInstance) where
  pretty :: SMTInstance -> Doc ann
  pretty (SMTInstance {conditions, problem}) =
    (if null conditions then "" else "Conditions:" <> line <> vsep (map pretty conditions) <> line)
      <> "Problem:"
      <> line
      <> pretty problem

invalidInstance :: SMTInstance
invalidInstance = SMTInstance {conditions = [], problem = Bot}

instance (Variables SMTInstance) where
  variables :: SMTInstance -> Set Text
  variables (SMTInstance {conditions, problem}) = foldl (\acc smt -> acc `Set.union` variables smt) (variables problem) conditions

instanceOf :: [SMTFormula] -> SMTFormula -> SMTInstance
instanceOf conditions problem = SMTInstance {conditions = conditions, problem = problem}