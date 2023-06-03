module Lexer.Basic (tokenize) where

import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

tokenize :: Tokenizer
tokenize input = case nextToken input of
    (Eof, _) -> [Eof]
    (t, xs) -> t : tokenize xs

nextToken :: String -> (Token, String)
nextToken [] = (Eof, [])
nextToken ('=' : '=' : xs) = (Equal, xs)
nextToken ('=' : xs) = (Assign, xs)
nextToken ('+' : xs) = (Plus, xs)
nextToken ('-' : xs) = (Minus, xs)
nextToken ('!' : '=' : xs) = (NotEqual, xs)
nextToken ('!' : xs) = (Bang, xs)
nextToken ('*' : xs) = (Asterisk, xs)
nextToken ('/' : xs) = (Slash, xs)
nextToken ('<' : xs) = (LessThan, xs)
nextToken ('>' : xs) = (GreaterThan, xs)
nextToken (',' : xs) = (Comma, xs)
nextToken (';' : xs) = (Semicolon, xs)
nextToken ('(' : xs) = (LParen, xs)
nextToken (')' : xs) = (RParen, xs)
nextToken ('{' : xs) = (LSquirly, xs)
nextToken ('}' : xs) = (RSquirly, xs)
nextToken input@(x : xs)
    | isIdentChar x = readIdent input
    | isDigit x = readInt input
    | isSpace x = nextToken xs
nextToken (_ : input) = (Illegal, input)

readIdent :: String -> (Token, String)
readIdent = first identToken . span isIdentChar

readInt :: String -> (Token, String)
readInt = first Int . span isDigit
