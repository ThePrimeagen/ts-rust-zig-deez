package root.ast.statements;

import root.Token;
import root.ast.expressions.Expression;

public class ReturnStatement extends Statement {

    private Expression returnValue;

    public ReturnStatement(Token token) {
        this.token = token;
    }

    public Expression getReturnValue() {
        return returnValue;
    }

    public void setReturnValue(Expression returnValue) {
        this.returnValue = returnValue;
    }

    @Override
    public String toString() {
        return "%s %s;".formatted(tokenLiteral(), returnValue);
    }
}
