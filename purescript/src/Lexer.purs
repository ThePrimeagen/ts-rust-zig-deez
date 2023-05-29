module Lexer where

import Prelude

import Control.Alt ((<|>))
import Data.CodePoint.Unicode (isLetter)
import Parsing (Parser, ParserT)
import Parsing.String (anyCodePoint, char, string)
import Parsing.String.Basic (number, takeWhile1)
import Token (Token(..))

illegalP :: forall a. ParserT String a Token
illegalP = (\_ -> Illegal) <$> (anyCodePoint)

letP :: forall a. ParserT String a Token
letP = (\_ -> Let) <$> (string "let")

intP :: forall a. ParserT String a Token
intP = Int <$> number

equalP :: forall a. ParserT String a Token
equalP = (\_ -> Equal) <$> (string "=")

identP :: forall a. ParserT String a Token
identP = Ident <$> takeWhile1 isLetter

semiColonP :: forall a. ParserT String a Token
semiColonP = (\_ -> Semicolon) <$> char ';'

fnP :: forall a. ParserT String a Token
fnP = (\_ -> Function) <$> string "fn"

plusP :: forall a. ParserT String a Token
plusP = (\_ -> Plus) <$> char '+'

commaP :: forall a. ParserT String a Token
commaP = (\_ -> Comma) <$> char ','

lParenP :: forall a. ParserT String a Token
lParenP = (\_ -> LParen) <$> char '('

rParenP :: forall a. ParserT String a Token
rParenP = (\_ -> RParen) <$> char ')'

lSquirlyP :: forall a. ParserT String a Token
lSquirlyP = (\_ -> LSquirly) <$> char '{'

rSquirlyP :: forall a. ParserT String a Token
rSquirlyP = (\_ -> RSquirly) <$> char '}'

token :: forall a. ParserT String a Token
token =
  letP
    <|> intP
    <|> equalP
    <|> identP
    <|> semiColonP
    <|> fnP
    <|> plusP
    <|> commaP
    <|> lParenP
    <|> rParenP
    <|> lSquirlyP
    <|> rSquirlyP
    <|> illegalP

parser :: Parser String Token
parser = token
