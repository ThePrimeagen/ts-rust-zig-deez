package jdsl

enum class TokenType {
    LSQUIRLY,
    RSQUIRLY,
    LPAREN,
    RPAREN,
    COMMA,
    SEMICOLON,
    PLUS,
    MINUS,
    SLASH,
    STAR,
    ASSIGN,
    IDENTIFIER,
    BANG,
    LESS,
    GREATER,

    NOT_EQ,
    EQ,
    LE,
    GE,

    STRING,
    NUMBER,

    ELSE,
    LET,
    FUNCTION,
    IF,
    RETURN,
    TRUE,
    FALSE,

    EOF,
    ILLEGAL
}