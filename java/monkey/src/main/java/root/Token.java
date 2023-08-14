package root;

public record Token(TokenType type, String literal) {

    public LocalizedToken localize(int line, int column, String codeLine) {
        return new LocalizedToken(this, line, column, codeLine);
    }
}
