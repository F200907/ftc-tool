{-# LANGUAGE OverloadedStrings #-}

module Data.ExpressionParser (parseBooleanExpr, parseArithmeticExpr) where

import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Expression
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (try), choice)
import Text.Megaparsec.Char (string)
import Util.ParserUtil

data BExpr
  = BTrue'
  | BFalse'
  | BNot' BExpr
  | BAnd' BExpr BExpr
  | BOr' BExpr BExpr
  | BImplies' BExpr BExpr
  | Equal' AExpr AExpr
  | NotEqual' AExpr AExpr
  | LessThan' AExpr AExpr
  | LessThanEqual' AExpr AExpr
  | GreaterThan' AExpr AExpr
  | GreaterThanEqual' AExpr AExpr
  deriving (Show)

data AExpr
  = Constant' Int
  | AVar' Text
  | LVar' Text
  | Negation' AExpr
  | Plus' AExpr AExpr
  | Minus' AExpr AExpr
  | Times' AExpr AExpr
  deriving (Show)

type AParser = Parser AExpr

type BParser = Parser BExpr

toArithmeticExpr :: AExpr -> ArithmeticExpr
toArithmeticExpr (Constant' c) = Constant c
toArithmeticExpr (AVar' x) = AVar x
toArithmeticExpr (LVar' x) = LVar x
toArithmeticExpr (Negation' a) = Negation (toArithmeticExpr a)
toArithmeticExpr (Plus' a b) = Plus (toArithmeticExpr a) (toArithmeticExpr b)
toArithmeticExpr (Minus' a b) = Minus (toArithmeticExpr a) (toArithmeticExpr b)
toArithmeticExpr (Times' a b) = Times (toArithmeticExpr a) (toArithmeticExpr b)

toBooleanExpr :: BExpr -> BooleanExpr
toBooleanExpr BTrue' = BTrue
toBooleanExpr BFalse' = BFalse
toBooleanExpr (BAnd' a b) = And (toBooleanExpr a) (toBooleanExpr b)
toBooleanExpr (BNot' a) = Not (toBooleanExpr a)
toBooleanExpr (BOr' a b) = Or (toBooleanExpr a) (toBooleanExpr b)
toBooleanExpr (BImplies' a b) = Or (Not (toBooleanExpr a)) (toBooleanExpr b)
toBooleanExpr (Equal' a b) = Equal (toArithmeticExpr a) (toArithmeticExpr b)
toBooleanExpr (NotEqual' a b) = Not (Equal (toArithmeticExpr a) (toArithmeticExpr b))
toBooleanExpr (LessThan' a b) = LessThan (toArithmeticExpr a) (toArithmeticExpr b)
toBooleanExpr (LessThanEqual' a b) = let a' = toArithmeticExpr a; b' = toArithmeticExpr b in Or (LessThan a' b') (Equal a' b')
toBooleanExpr (GreaterThan' a b) = Not (toBooleanExpr (LessThanEqual' a b))
toBooleanExpr (GreaterThanEqual' a b) = Not (toBooleanExpr (LessThan' a b))

minusToken :: Text
minusToken = "-"

plusToken :: Text
plusToken = "+"

timesToken :: Text
timesToken = "*"

trueToken :: Text
trueToken = "true"

falseToken :: Text
falseToken = "false"

andToken :: Text
andToken = "&"

orToken :: Text
orToken = "|"

notToken :: Text
notToken = "!"

equalToken :: Text
equalToken = "="

lessToken :: Text
lessToken = "<"

leqToken :: Text
leqToken = "<="

greaterToken :: Text
greaterToken = ">"

geqToken :: Text
geqToken = ">="

neqToken :: Text
neqToken = "!="

impliesToken :: Text
impliesToken = "=>"

pLVariable :: AParser
pLVariable = LVar' <$> lexeme (do
  _ <- symbol "#"
  identifier)

pAVariable :: AParser
pAVariable = AVar' <$> lexeme identifier

pVariable :: AParser
pVariable = choice [pAVariable, pLVariable]

pConstant :: AParser
pConstant = Constant' <$> integer

pATerm :: AParser
pATerm =
  choice
    [ parens pAExp,
      pVariable,
      pConstant
    ]

arithmeticOperatorTable :: [[Operator Parser AExpr]]
arithmeticOperatorTable =
  [ [ prefix minusToken Negation',
      prefix plusToken id
    ],
    [binary timesToken Times'],
    [ binary plusToken Plus',
      binary minusToken Minus'
    ]
  ]

pAExp :: AParser
pAExp = makeExprParser pATerm arithmeticOperatorTable

parseArithmeticExpr :: Parser ArithmeticExpr
parseArithmeticExpr = toArithmeticExpr <$> pAExp

pTrue :: BParser
pTrue = do
  _ <- lexeme (string trueToken)
  return BTrue'

pFalse :: BParser
pFalse = do
  _ <- lexeme (string falseToken)
  return BFalse'

pRelation :: Text -> (AExpr -> AExpr -> BExpr) -> BParser
pRelation name relation = try $ do
  a <- pAExp
  _ <- symbol name
  relation a <$> pAExp

pBTerm :: BParser
pBTerm =
  choice
    [ parens pBExp,
      pRelation equalToken Equal',
      pRelation neqToken NotEqual',
      pRelation lessToken LessThan',
      pRelation leqToken LessThanEqual',
      pRelation greaterToken GreaterThan',
      pRelation geqToken GreaterThanEqual',
      pTrue,
      pFalse
    ]

booleanOperatorTable :: [[Operator Parser BExpr]]
booleanOperatorTable =
  [ [prefix notToken BNot'],
    [ binary andToken BAnd',
      binary orToken BOr'],
      [binary impliesToken BImplies']
  ]

pBExp :: BParser
pBExp = makeExprParser pBTerm booleanOperatorTable

parseBooleanExpr :: Parser BooleanExpr
parseBooleanExpr = toBooleanExpr <$> pBExp