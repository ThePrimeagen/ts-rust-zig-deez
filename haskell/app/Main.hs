module Main where

import Lexer qualified (Token (..), tokenize)

main :: IO ()
main = interact $ show . takeWhile (/= Lexer.Eof) . Lexer.tokenize
