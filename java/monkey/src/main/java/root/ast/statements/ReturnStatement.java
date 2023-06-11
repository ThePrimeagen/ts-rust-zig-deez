package root.ast.statements;

import root.Token;
import root.ast.expressions.Expression;

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
