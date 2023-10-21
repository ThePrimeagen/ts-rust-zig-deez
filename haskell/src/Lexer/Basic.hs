module Lexer.Basic (tokenize) where

{-
                          README
----------------------------------------------------------
This Lexer is implemented using the noobest technique but
the easiest to understand. Our input is a linked list of
characters (in Haskell a String) and we transform it into
a linked list of Token by directly pattern matching on the
list.

- This implementation isn't book compliant
- src/Lexer/Parsec.hs is the most idiomatic the technique

-}

import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

tokenize :: Tokenizer                    -- A Tokenizer is a type synonym for a function from String to [Token]
tokenize input = case nextToken input of -- recursively call nextToken until you consume the list of characters
    (Eof, _) -> [Eof]
    (t, xs) -> t : tokenize xs           -- symbol : means prepend; hence t : tokenize xs means prepend t to the result of tokenize xs

-- Common pattern: return the token and the string having comsumed the chars of such a token
nextToken :: String -> (Token, String)       
nextToken [] = (Eof, [])                     -- Pattern matching the empty string
nextToken ('=' : '=' : xs) = (Equal, xs)     -- Pattern matching on first two elements. Rust equiv: ['=', '=', xs @ ..]
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
readIdent = first identToken . span isIdentChar -- split the list in two based on isIdentChar; then apply identToken to first component
--                           |- this dot is function composition: f . g is equivalent f(g(x)) in other languages
readInt :: String -> (Token, String)
readInt = first Int . span isDigit
