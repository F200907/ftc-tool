{-# LANGUAGE OverloadedStrings #-}

module Util.ParserUtil (Parser, sc, lexeme, symbol, integer, identifier, parens, brackets, braces, binary, prefix, tryWith) where

import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix))
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

identifier :: Parser Text
identifier = pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

tryWith :: (Parser a -> Parser a) -> Parser a -> Parser a
tryWith f p = try (f p) <|> p