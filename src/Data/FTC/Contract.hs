module Data.FTC.Contract (Contract, Contracts, pre, post, lookupContract, trueContract) where
import Data.Expression (BooleanExpr (BTrue))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

type Contract = (BooleanExpr, BooleanExpr)

type Contracts = Map Text Contract

lookupContract :: Text -> Contracts -> Maybe Contract
lookupContract = Map.lookup

pre :: Contract -> BooleanExpr
pre (c, _) = c

post :: Contract -> BooleanExpr
post (_, c) = c


trueContract :: Contract
trueContract = (BTrue, BTrue)