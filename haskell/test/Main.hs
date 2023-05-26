module Main where

import LexerTest (lexerTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTestTT lexerTests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess
