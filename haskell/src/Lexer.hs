module Lexer (tokenize, Token (..)) where

import Data.Char (isDigit, isLetter, isSpace)

data Token
    = Ident String
    | Int String
    | Illegal
    | Eof
    | Equal
    | Comma
    | Plus
    | Semicolon
    | LParen
    | RParen
    | LSquirly
    | RSquirly
    | Function
    | Let
    deriving (Show, Eq)

data Lexer = Lexer
    { input :: String
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

newLexer :: String -> Lexer
newLexer input =
    readChar
        Lexer
            { input = input
            , position = 0
            , readPosition = 0
            , ch = '\0'
            }

tokenize :: String -> [Token]
tokenize input = tokenize' $ newLexer input

tokenize' :: Lexer -> [Token]
tokenize' lexer = tok : tokenize' lexer'
  where
    (lexer', tok) = nextToken lexer

nextToken :: Lexer -> (Lexer, Token)
nextToken lexer =
    let
        lexer' = skipWhitespace lexer
        (lexer'', tok) = case ch lexer' of
            '=' -> (readChar lexer', Equal)
            ';' -> (readChar lexer', Semicolon)
            '(' -> (readChar lexer', LParen)
            ')' -> (readChar lexer', RParen)
            ',' -> (readChar lexer', Comma)
            '+' -> (readChar lexer', Plus)
            '{' -> (readChar lexer', LSquirly)
            '}' -> (readChar lexer', RSquirly)
            '\0' -> (readChar lexer', Eof)
            _ ->
                if ch lexer' `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
                    then
                        let (lexer''', ident) = readIdent lexer'
                         in case ident of
                                "fn" -> (lexer''', Function)
                                "let" -> (lexer''', Let)
                                _ -> (lexer''', Ident ident)
                    else
                        if isDigit $ ch lexer'
                            then Int <$> readInt lexer'
                            else (lexer', Illegal)
     in
        (lexer'', tok)

readChar :: Lexer -> Lexer
readChar lexer@Lexer{input, readPosition} =
    lexer
        { ch = if readPosition >= length input then '\0' else input !! readPosition
        , position = readPosition
        , readPosition = readPosition + 1
        }

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer@Lexer{ch}
    | isSpace ch = skipWhitespace $ readChar lexer
    | otherwise = lexer

skipIdentChars :: Lexer -> Lexer
skipIdentChars lexer@Lexer{ch}
    | isLetter ch || ch == '_' = skipIdentChars $ readChar lexer
    | otherwise = lexer

readIdent :: Lexer -> (Lexer, String)
readIdent lexer@Lexer{input, position = pos} = (lexer', ident)
  where
    lexer' = skipIdentChars lexer
    ident = take (position lexer' - pos) $ drop pos input

skipIntChars :: Lexer -> Lexer
skipIntChars lexer@Lexer{ch}
    | isDigit ch = skipIntChars $ readChar lexer
    | otherwise = lexer

readInt :: Lexer -> (Lexer, String)
readInt lexer@Lexer{input, position = pos} = (lexer', int)
  where
    lexer' = skipIntChars lexer
    int = take (position lexer' - pos) $ drop pos input
