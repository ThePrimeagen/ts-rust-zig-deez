from dataclasses import dataclass

from monkey.token_types import *


@dataclass
class Token:
    type: TokenType
    literal: str

    def __init__(self, token_type: TokenType, literal):
        self.type = token_type
        self.literal = literal


RESERVED_KEYWORDS = {
    "fn": Token(FUNCTION, "fn"),
    "let": Token(LET, "let"),
    "true": Token(TRUE, "true"),
    "false": Token(FALSE, "false"),
    "if": Token(IF, "if"),
    "else": Token(ELSE, "else"),
    "return": Token(RETURN, "return"),
}

FIXED_TOKENS = {
    "=": Token(ASSING, ASSING),
    "+": Token(PLUS, PLUS),
    "-": Token(MINUS, MINUS),
    "!": Token(BANG, BANG),
    "/": Token(SLASH, SLASH),
    "*": Token(ASTERISK, ASTERISK),
    "<": Token(LESSTHAN, LESSTHAN),
    ">": Token(GREATERTHAN, GREATERTHAN),
    "!=": Token(NOTEQUAL, NOTEQUAL),
    "==": Token(EQUAL, EQUAL),
    ",": Token(COMMA, COMMA),
    ";": Token(SEMICOLON, SEMICOLON),
    "(": Token(LPAREN, LPAREN),
    ")": Token(RPAREN, RPAREN),
    "{": Token(LSQUIRLY, LSQUIRLY),
    "}": Token(RSQUIRLY, RSQUIRLY),
    "[": Token(LBRACE, LBRACE),
    "]": Token(RBRACE, RBRACE),
    "\0": Token(EOF, EOF),
}
