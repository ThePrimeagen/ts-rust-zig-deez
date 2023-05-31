module Main where

import Lexer.Basic qualified (tokenize)

main :: IO ()
main = interact $ show . Lexer.Basic.tokenize
