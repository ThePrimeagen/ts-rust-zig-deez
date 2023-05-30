from enum import Enum
from dataclasses import dataclass


class TokenType(Enum):
    ILLEGAL = "ILLEGAL"
    EOF = "EOF"

    # Identifiers + literals
    IDENT = "IDENT"  # add, foobar, x, y, ...
    INT = "INT"  # 1343456

    # Delimiters
    COMMA = ","
    SEMICOLON = ";"
    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"

    # Keywords
    FUNCTION = "FUNCTION"
    LET = "LET"
    # Operators
    ASSIGN = "="
    PLUS = "+"
    MINUS = "-"
    BANG = "!"
    ASTERISK = "*"
    SLASH = "/"
    LT = "<"
    GT = ">"
    TRUE = "true",
    FALSE = "false",
    IF = "if",
    ELSE = "else",
    RETURN = "return",
    EQ = "==",
    NOT_EQ = "!=",


token_dict = {
    ';': TokenType.SEMICOLON,
    '(': TokenType.LPAREN,
    ')': TokenType.RPAREN,
    ',': TokenType.COMMA,
    '+': TokenType.PLUS,
    '{': TokenType.LBRACE,
    '}': TokenType.RBRACE,
    '-': TokenType.MINUS,
    '/': TokenType.SLASH,
    '*': TokenType.ASTERISK,
    '<': TokenType.LT,
    '>': TokenType.GT,

}

keywords = {
    "fn": TokenType.FUNCTION,
    "let": TokenType.LET,
    "true": TokenType.TRUE,
    "false": TokenType.FALSE,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "return": TokenType.RETURN,
}


@ dataclass
class Token:
    Type: TokenType
    Literal: str
