import java.util.Optional;

public enum TokenType {
    ILLEGAL,
    EOF("eof"),
    IDENT,
    INT,
    COMMA(","),
    SEMI(";"),
    LPAREN("("),
    RPAREN(")"),
    LSQIRLY("{"),
    RSQIRLY("}"),

    // Operations
    EQUAL("="),
    PLUS("+"),
    MINUS("-"),
    BANG("!"),
    ASTERISK("*"),
    SLASH("/"),
    LT("<"),
    GT(">"),

    // Keywords
    FUNC("fn"),
    LET("let");

    private final Optional<Token> token;

    TokenType(String literal) {
        token = Optional.of(new Token(this, literal));
    }

    TokenType() {
        this.token = Optional.empty();
    }

    public Token createToken(String literal) {
        return new Token(this, literal);
    }

    public Token token() {
        return token.orElseThrow(() -> new IllegalArgumentException(
                "TokenType %s doesn't have a default Token. Create one using 'createToken'".formatted(this.name())
        ));
    }
}