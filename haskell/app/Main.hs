module Main where

import Lexer qualified (Token (..), mkLexer, nextToken)

tokenize :: String -> [Lexer.Token]
tokenize = go . Lexer.mkLexer
  where
    go lexer = case Lexer.nextToken lexer of
        (_, Lexer.Eof) -> [Lexer.Eof]
        (lexer', token) -> token : go lexer'

main :: IO ()
main = interact $ show . tokenize
