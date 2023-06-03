module main

pub enum TokenType {
    Illegal = "ILLEGAL"
    Eof = "EOF"
    Ident = "IDENT"
    Int = "INT"
    Equal = "="
    Plus = "+"
    Comma = ","
    Semicolon = ";"
    LParen = "("
    RParen = ")"
    LSquirly = "{"
    RSquirly = "}"
    Function = "FUNCTION"
    Let = "LET"
}

pub char_to_token_type := {
    '=': TokenType.Equal,
    '+': TokenType.Plus,
    ',': TokenType.Comma,
    ';': TokenType.Semicolon,
    '(': TokenType.LParen,
    ')': TokenType.RParen,
    '{': TokenType.LSquirly,
    '}': TokenType.RSquirly,
    '\x00': TokenType.Eof
}

pub struct Token {
    type TokenType
    literal string
}
