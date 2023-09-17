package root;

public record LocalizedToken(Token token, int line, int column, String codeLine) {

    public String literal() {
        return token.literal();
    }

    public TokenType type() {
        return token.type();
    }
}
