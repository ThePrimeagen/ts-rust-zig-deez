{-# LANGUAGE OverloadedStrings #-}

module Lexer.Parsec where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, empty, eof, errorBundlePretty, many, parse, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Lexer = Parsec Void Text

tokenize :: Tokenizer
tokenize input = case parse tokensP "" $ T.pack input of
    Left err -> error ("lexer should not fail" ++ errorBundlePretty err)
    Right tokens -> tokens ++ [Eof]
  where
    tokensP = sc *> many nextToken <* eof

nextToken :: Lexer Token
nextToken =
    choice
        [ LSquirly <$ symbol "{"
        , RSquirly <$ symbol "}"
        , LParen <$ symbol "("
        , RParen <$ symbol ")"
        , Comma <$ symbol ","
        , Semicolon <$ symbol ";"
        , Plus <$ symbol "+"
        , Minus <$ symbol "-"
        , try (NotEqual <$ symbol "!=") <|> (Bang <$ symbol "!")
        , GreaterThan <$ symbol ">"
        , LessThan <$ symbol "<"
        , Asterisk <$ symbol "*"
        , Slash <$ symbol "/"
        , try (Equal <$ symbol "==") <|> (Assign <$ symbol "=")
        , identToken <$> identP
        , Int <$> intP
        , Illegal <$ lexeme anySingle
        ]

identP :: Lexer String
identP = T.unpack <$> lexeme (takeWhile1P Nothing isIdentChar)

intP :: Lexer String
intP = T.unpack <$> lexeme (takeWhile1P Nothing isDigit)

sc :: Lexer ()
sc = L.space space1 empty empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc
