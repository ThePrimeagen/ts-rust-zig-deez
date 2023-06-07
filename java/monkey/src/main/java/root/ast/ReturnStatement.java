package root.ast;

import root.Token;

public class ReturnStatement extends Statement {

    public Expression returnValue;

    public ReturnStatement(Token token) {
        this.token = token;
    }

    @Override
    public String toString() {
        return "%s %s;".formatted(tokenLiteral(), returnValue);
    }
}
