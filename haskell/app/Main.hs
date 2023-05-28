module Main where

import Lexer qualified (tokenize)

main :: IO ()
main = interact $ show . Lexer.tokenize
