from enum import Enum
from dataclasses import dataclass

class TokenType(Enum):
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

char_to_token_type = {
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

@dataclass
class Token:
    type: TokenType
    literal: str
