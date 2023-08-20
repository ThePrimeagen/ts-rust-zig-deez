package root;

public enum TokenType {
    ILLEGAL,
    EOF("eof"),
    IDENTIFIER,
    INT,
    COMMA(","),
    SEMI(";"),
    LPAREN("("),
    RPAREN(")"),
    LSQIRLY("{"),
    RSQIRLY("}"),

    // Operations
    ASSIGN("="),
    PLUS("+"),
    MINUS("-"),
    BANG("!"),
    ASTERISK("*"),
    SLASH("/"),
    LT("<"),
    GT(">"),
    EQUAL("=="),
    NOT_EQUAL("!="),

    // Keywords
    FUNC("fn"),
    LET("let"),
    TRUE("true"),
    FALSE("false"),
    IF("if"),
    ELSE("else"),
    RETURN("return"),
    NULL("null"); // My additon.

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
        if (token == null) {
            throw new IllegalArgumentException(
                "TokenType %s doesn't have a default Token. Create one using 'createToken'".formatted(this.name())
            );
        }
        return token;
    }

    public String tokenOrName() {
        return token == null ? name() : token.literal();
    }
}
