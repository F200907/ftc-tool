{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module SMT.Instance (SMTInstance (..), invalidInstance, instanceOf) where

import Data.Text (Text)
import SMT.Formula (SMTFormula (..))
import Data.Expression (Variables(..))
import Data.Set (Set)

data SMTInstance = SMTInstance {conditions :: [SMTFormula], problem :: SMTFormula} deriving (Show)

invalidInstance :: SMTInstance
invalidInstance = SMTInstance {conditions = [], problem = Bot}

instance (Variables SMTInstance) where
    variables :: SMTInstance -> Set Text
    variables (SMTInstance {problem}) = variables problem

instanceOf :: [SMTFormula] -> SMTFormula -> SMTInstance
instanceOf conditions problem = SMTInstance {conditions = conditions, problem = problem}