module token;

alias TokenType = string;

struct Token {
    TokenType type;
    string literal;

    this(TokenType type, string literal) {
        this.type = type;
        this.literal = literal;
    }
    this(TokenType type, char literal) {
        this.type = type;
        this.literal = literal.stringof;
    }
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
