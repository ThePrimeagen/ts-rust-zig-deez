public enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    EQUAL,
    PLUS,
    COMMA,
    SEMI,
    LPAREN,
    RPAREN,
    LSQIRLY,
    RSQIRLY,
    FUNC,
    LET;

    public Token createToken(String literal) {
        return new Token(this, literal);
    }
}