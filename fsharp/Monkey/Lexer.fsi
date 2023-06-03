module Monkey.Lexer

type Token =
    | Ident of string
    | Int of int
    | Illegal
    | Eof
    | Equal
    | Plus
    | Comma
    | Semicolon
    | LParen
    | RParen
    | LSquirly
    | RSquirly
    | Function
    | Let

val tokenize: string -> Token seq
