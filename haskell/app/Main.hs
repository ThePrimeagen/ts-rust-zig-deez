module Main where

import Lexer.Basic qualified (tokenize)
import Lexer.Monad qualified (tokenize)
import Lexer.Parsec qualified (tokenize)
import Lexer.State qualified (tokenize)

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Token (Tokenizer)

main :: IO ()
main = getArgs >>= parse >>= \lexer -> interact $ show . chooseLexer lexer

chooseLexer :: String -> Tokenizer
chooseLexer "basic" = Lexer.Basic.tokenize
chooseLexer "monad" = Lexer.Monad.tokenize
chooseLexer "state" = Lexer.State.tokenize
chooseLexer "parsec" = Lexer.Parsec.tokenize
chooseLexer _ = error "Unknown lexer: use basic, monad, state or parsec"

parse :: [[Char]] -> IO String
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse [] = read <$> getContents
parse [lexer] = return lexer
parse _ = usage >> exitWith (ExitFailure 1)

usage :: IO ()
usage = putStrLn "Usage: haskell [-vh] <lexer>"

version :: IO ()
version = putStrLn "Monkey Lexer"
