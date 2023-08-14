package root.ast.statements;

import root.LocalizedToken;
import root.Token;
import root.ast.expressions.Expression;

public class ExpressionStatement extends Statement {

    private Expression expression;

    public ExpressionStatement(LocalizedToken token) {
        this.token = token;
    }

    public Expression getExpression() {
        return expression;
    }

    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    @Override
    public String toString() {
        return String.valueOf(expression);
    }
}
