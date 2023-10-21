module Token where

import Data.Char (isLetter)

type Tokenizer = String -> [Token] -- type synonym. A function from String to Linked list of Token

data Token           -- enum Token {
    = Illegal        --  Illegal,
    | Eof            --  Eof,
    | Ident String   --  Ident(String),
    | Int String     -- ...
    | Assign
    | Plus
    | Minus
    | Bang
    | Asterisk
    | Slash
    | LessThan
    | GreaterThan
    | Equal
    | NotEqual
    | Comma
    | Semicolon
    | LParen
    | RParen
    | LSquirly
    | RSquirly
    | Function
    | Let
    | TrueTok
    | FalseTok
    | If
    | Else
    | Return
    deriving (Show, Eq, Ord)  -- #[derive(Debug, PartialEq, Eq)]

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || c == '_' -- valid identifier characters

identToken :: String -> Token    -- Simple Pattern matching
identToken "fn" = Function
identToken "let" = Let
identToken "true" = TrueTok
identToken "false" = FalseTok
identToken "if" = If
identToken "else" = Else
identToken "return" = Return
identToken x = Ident x
