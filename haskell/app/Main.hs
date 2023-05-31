module Main where

import Lexer.Basic qualified (tokenize)
import Lexer.Monad qualified (tokenize)

main :: IO ()
main = interact $ show . Lexer.Basic.tokenize
