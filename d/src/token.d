module token;

alias TokenType = string;

struct Token {
    TokenType type;
    string literal;
}

enum {
    ILLEGAL = "ILLEGAL",
    EOF = "EOF",
    
    // Identifiers + literals
    IDENT = "IDENT", // add, foobar, x, y, ...
    INT = "INT", // 1343456

    // Operators
    ASSIGN = "=",
    PLUS = "+",
    
    COMMA = ",",
    SEMICOLON = ";",

    LPAREN = "(",
    RPAREN = ")",
    LBRACE = "{",
    RBRACE = "}",

    // Keywords
    FUNCTION = "FUNCTION",
    LET = "LET"
}
