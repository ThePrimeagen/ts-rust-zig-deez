module Main where

import Lexer.Basic qualified (tokenize)
import Lexer.Monad qualified (tokenize)
import Lexer.State qualified (tokenize)

main :: IO ()
main = interact $ show . Lexer.Basic.tokenize
