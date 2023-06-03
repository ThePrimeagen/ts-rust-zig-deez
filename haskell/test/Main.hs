module Main where

import Lexer.Basic qualified
import Lexer.Book qualified
import Lexer.Lens qualified
import Lexer.Monad qualified
import Lexer.Parsec qualified
import Lexer.State qualified
import LexerTest (lexerTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

tests :: Test
tests =
    TestList
        [ TestLabel "Lexer.Basic" (lexerTests Lexer.Basic.tokenize)
        , TestLabel "Lexer.Book" (lexerTests Lexer.Book.tokenize)
        , TestLabel "Lexer.Monad" (lexerTests Lexer.Monad.tokenize)
        , TestLabel "Lexer.State" (lexerTests Lexer.State.tokenize)
        , TestLabel "Lexer.Parsec" (lexerTests Lexer.Parsec.tokenize)
        , TestLabel "Lexer.Lens" (lexerTests Lexer.Lens.tokenize)
        ]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT tests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess
