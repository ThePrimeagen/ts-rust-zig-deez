enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT, // 1343456

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
}


class Token {
    TokenType Type
    String Literal

    Token(TokenType type, String literal) {
        this.Type = type
        this.Literal = literal
    }

    Token(TokenType type, byte bliteral) {
        this.Type = type;
        this.Literal = new String(bliteral);
    }
}

