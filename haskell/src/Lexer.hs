module Lexer (tokenize, Token (..)) where

import Data.Char (isDigit, isLetter, isSpace)

data Token
    = Ident String
    | Int String
    | Illegal
    | Eof
    | Equal
    | Comma
    | Plus
    | Semicolon
    | LParen
    | RParen
    | LSquirly
    | RSquirly
    | Function
    | Let
    deriving (Show, Eq)

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || c == '_'

tokenize :: String -> [Token]
tokenize input = lexer input []

lexer :: String -> [Token] -> [Token]
lexer [] tokens = reverse $ Eof : tokens
lexer input@(x : xs) tokens
    | isSpace x = lexer xs tokens
    | otherwise = lexer rest (token : tokens)
  where
    (token, rest) = nextToken input

nextToken :: String -> (Token, String)
nextToken [] = (Eof, [])
nextToken ('=' : xs) = (Equal, xs)
nextToken (',' : xs) = (Comma, xs)
nextToken ('+' : xs) = (Plus, xs)
nextToken (';' : xs) = (Semicolon, xs)
nextToken ('(' : xs) = (LParen, xs)
nextToken (')' : xs) = (RParen, xs)
nextToken ('{' : xs) = (LSquirly, xs)
nextToken ('}' : xs) = (RSquirly, xs)
nextToken input@(x : xs)
    | isIdentChar x = readIdent input
    | isDigit x = readInt input
    | otherwise = (Illegal, xs)

readIdent :: String -> (Token, String)
readIdent input = (token, rest)
  where
    (ident, rest) = span isIdentChar input
    token = case ident of
        "fn" -> Function
        "let" -> Let
        _ -> Ident ident

readInt :: String -> (Token, String)
readInt input = (Int int, rest)
  where
    (int, rest) = span isDigit input
