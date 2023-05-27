module Lexer (mkLexer, nextToken, Token (..)) where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isDigit, isLetter, isSpace)
import Token (Token (..), identToken)

data Lexer = Lexer
    { inputLoc :: Int
    , inputStr :: String
    }
    deriving (Show, Eq)

advance :: Lexer -> Lexer
advance (Lexer loc []) = Lexer loc []
advance (Lexer loc (_ : xs)) = Lexer (loc + 1) xs

peek :: Lexer -> Char
peek (Lexer _ []) = '\0'
peek (Lexer _ (x : _)) = x

mkLexer :: String -> Lexer
mkLexer = Lexer 0

nextToken :: Lexer -> (Lexer, Token)
nextToken (Lexer loc []) = (Lexer loc [], Eof)
nextToken lexer = case peek lexer of
    '=' ->
        let lexer' = advance lexer
         in case peek lexer' of
                '=' -> (advance lexer', Equal)
                _ -> (lexer', Assign)
    '+' -> (advance lexer, Plus)
    '-' -> (advance lexer, Minus)
    '!' ->
        let lexer' = advance lexer
         in case peek lexer' of
                '=' -> (advance lexer', NotEqual)
                _ -> (lexer', Bang)
    '*' -> (advance lexer, Asterisk)
    '/' -> (advance lexer, Slash)
    '<' -> (advance lexer, LessThan)
    '>' -> (advance lexer, GreaterThan)
    ',' -> (advance lexer, Comma)
    ';' -> (advance lexer, Semicolon)
    '(' -> (advance lexer, LParen)
    ')' -> (advance lexer, RParen)
    '{' -> (advance lexer, LSquirly)
    '}' -> (advance lexer, RSquirly)
    x
        | isSpace x -> nextToken $ advance lexer
        | isIdentChar x -> readIdent lexer
        | isDigit x -> readInt lexer
    _ -> (advance lexer, Illegal)

readIdent :: Lexer -> (Lexer, Token)
readIdent lexer = identToken <$> until (not . isIdentChar . peek . fst) step (lexer, "")

readInt :: Lexer -> (Lexer, Token)
readInt lexer = Int <$> until (not . isDigit . peek . fst) step (lexer, "")

step :: (Lexer, String) -> (Lexer, String)
step = uncurry (flip fmap) . bimap (advance &&& ((: []) . peek)) (++)

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || c == '_'
