module Main where

import Lexer.Basic qualified
import Lexer.Monad qualified
import LexerTest (lexerTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

tests :: Test
tests =
    TestList
        [ TestLabel "Lexer" (lexerTests Lexer.Basic.tokenize)
        , TestLabel "Lexer.Monad" (lexerTests Lexer.Monad.tokenize)
        ]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT tests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess