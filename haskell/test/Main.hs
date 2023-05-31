module Main where

import Lexer.Basic qualified
import LexerTest (lexerTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

tests :: Test
tests =
    TestList
        [ TestLabel "Lexer.Basic" (lexerTests Lexer.Basic.tokenize)
        ]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT tests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess
