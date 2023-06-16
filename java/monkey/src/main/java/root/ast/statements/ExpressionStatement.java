package root.ast.statements;

import root.ast.expressions.Expression;

public class ExpressionStatement extends Statement {

    private Expression expression;

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
