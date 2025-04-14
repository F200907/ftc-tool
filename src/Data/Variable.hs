module Data.Variable (VariableName, Variables (..), Renameable (..), fresh) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

type VariableName = Text

class Variables a where
  variables :: a -> Set Text

class Renameable a where
  rename :: a -> VariableName -> VariableName -> a

fresh :: Set Text -> Text
fresh xs =
  let prefix = '$'
   in head $ filter (not . (`Set.member` xs)) $ map (T.cons prefix . T.pack . show) ([0 ..] :: [Int])