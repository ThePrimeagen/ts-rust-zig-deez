module Main where

import Lexer.Basic qualified
import Lexer.Book qualified
import Lexer.Lens qualified
import Lexer.Monad qualified
import Lexer.Parsec qualified
import Lexer.State qualified

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Token (Tokenizer)

main :: IO ()
main = getArgs >>= parse >>= \lexer -> interact $ show . chooseLexer lexer

chooseLexer :: String -> Tokenizer
chooseLexer "basic" = Lexer.Basic.tokenize
chooseLexer "book" = Lexer.Book.tokenize
chooseLexer "monad" = Lexer.Monad.tokenize
chooseLexer "state" = Lexer.State.tokenize
chooseLexer "parsec" = Lexer.Parsec.tokenize
chooseLexer "lens" = Lexer.Lens.tokenize
chooseLexer _ = error "Unknown lexer: use basic, book, monad, state, parsec or lens"

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
