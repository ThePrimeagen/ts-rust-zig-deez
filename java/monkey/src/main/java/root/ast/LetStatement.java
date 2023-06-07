package root.ast;

import root.Token;

public class LetStatement extends Statement {

    public Identifier name;
    public Expression value;

    public LetStatement(Token token) {
        this.token = token;
    }

    @Override
    public String toString() {
        return "%s %s = %s;".formatted(tokenLiteral(), name, value);
    }
}
