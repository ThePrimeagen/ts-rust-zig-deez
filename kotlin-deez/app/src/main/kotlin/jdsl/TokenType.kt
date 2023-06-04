package jdsl

enum class TokenType {
    LSQUIRLY,
    RSQUIRLY,
    LPAREN,
    RPAREN,
    COMMA,
    SEMICOLON,
    PLUS,
    EQUAL,
    IDENTIFIER,

    STRING,
    NUMBER,

    ELSE,
    LET,
    FUNCTION,
    IF,
    RETURN,

    EOF,
    ILLEGAL
}