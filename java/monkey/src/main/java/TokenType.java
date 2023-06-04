public enum TokenType {
    ILLEGAL,
    EOF("eof"),
    IDENT,
    INT,
    EQUAL("="),
    PLUS("+"),
    COMMA(","),
    SEMI(";"),
    LPAREN("("),
    RPAREN(")"),
    LSQIRLY("{"),
    RSQIRLY("}"),
    FUNC("fn"),
    LET("let");

    private final Token token;

    TokenType(String literal) {
        token = new Token(this, literal);
    }

    TokenType() {
        this.token = null;
    }

    public Token createToken(String literal) {
        return new Token(this, literal);
    }

    public Token token() {
        return token;
    }
}
