module Token where

type Tokenizer = String -> [Token]

data Token
    = Illegal
    | Eof
    | Ident String
    | Int String
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
    deriving (Show, Eq)

identToken :: String -> Token
identToken "fn" = Function
identToken "let" = Let
identToken "true" = TrueTok
identToken "false" = FalseTok
identToken "if" = If
identToken "else" = Else
identToken "return" = Return
identToken x = Ident x
