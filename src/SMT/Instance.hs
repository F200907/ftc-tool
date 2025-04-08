{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SMT.Instance (SMTInstance (..), invalidInstance, instanceOf) where

import Data.Expression (Variables (..))
import Data.Set (Set)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (..), line, vsep)
import SMT.Formula (SMTFormula (..))

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
  variables (SMTInstance {problem}) = variables problem

instanceOf :: [SMTFormula] -> SMTFormula -> SMTInstance
instanceOf conditions problem = SMTInstance {conditions = conditions, problem = problem}