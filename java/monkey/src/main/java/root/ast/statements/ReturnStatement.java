package root.ast.statements;

import root.LocalizedToken;
import root.ast.expressions.Expression;

public class ReturnStatement extends Statement {

    private Expression returnValue;

    public ReturnStatement(LocalizedToken token) {
        super(token);
    }

    public Expression getReturnValue() {
        return returnValue;
    }

    public void setReturnValue(Expression returnValue) {
        this.returnValue = returnValue;
    }

    @Override
    public String stringRep() {
        return "%s %s;".formatted(tokenLiteral(), returnValue.stringRep());
    }
}
