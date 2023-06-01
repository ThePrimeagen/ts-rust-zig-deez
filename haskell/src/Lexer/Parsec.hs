{-# LANGUAGE OverloadedStrings #-}

module Lexer.Parsec where

import Data.Char (isDigit, isLetter)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, empty, eof, errorBundlePretty, many, parse, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Token (Token (..), Tokenizer, identToken)

tokenize :: Tokenizer
tokenize input = case parse tokensP "" $ T.pack input of
    Left err -> error ("lexer should not fail" ++ errorBundlePretty err)
    Right tokens -> tokens ++ [Eof]

type Lexer = Parsec Void Text

sc :: Lexer ()
sc = L.space space1 empty empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

lSquirlyP :: Lexer Token
lSquirlyP = LSquirly <$ symbol "{"

rSquirlyP :: Lexer Token
rSquirlyP = RSquirly <$ symbol "}"

lParenP :: Lexer Token
lParenP = LParen <$ symbol "("

rParenP :: Lexer Token
rParenP = RParen <$ symbol ")"

commaP :: Lexer Token
commaP = Comma <$ symbol ","

semicolonP :: Lexer Token
semicolonP = Semicolon <$ symbol ";"

plusP :: Lexer Token
plusP = Plus <$ symbol "+"

minusP :: Lexer Token
minusP = Minus <$ symbol "-"

notEqualOrBangP :: Lexer Token
notEqualOrBangP = try (NotEqual <$ symbol "!=") <|> (Bang <$ symbol "!")

greaterThanP :: Lexer Token
greaterThanP = GreaterThan <$ symbol ">"

lessThanP :: Lexer Token
lessThanP = LessThan <$ symbol "<"

asteriskP :: Lexer Token
asteriskP = Asterisk <$ symbol "*"

slashP :: Lexer Token
slashP = Slash <$ symbol "/"

assignOrEqualP :: Lexer Token
assignOrEqualP = try (Equal <$ symbol "==") <|> (Assign <$ symbol "=")

identP :: Lexer Token
identP = identToken . T.unpack <$> lexeme (takeWhile1P Nothing isIdentChar)

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || c == '_'

intP :: Lexer Token
intP = Int . T.unpack <$> lexeme (takeWhile1P Nothing isDigit)

illegalP :: Lexer Token
illegalP = Illegal <$ lexeme anySingle

tokenP :: Lexer Token
tokenP =
    choice
        [ lSquirlyP
        , rSquirlyP
        , lParenP
        , rParenP
        , commaP
        , semicolonP
        , plusP
        , minusP
        , notEqualOrBangP
        , greaterThanP
        , lessThanP
        , asteriskP
        , slashP
        , assignOrEqualP
        , identP
        , intP
        , illegalP
        ]

tokensP :: Lexer [Token]
tokensP = sc *> many tokenP <* eof
