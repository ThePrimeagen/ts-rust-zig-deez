package root.ast.statements;

import root.ast.expressions.Expression;

public class ExpressionStatement extends Statement {

    public Expression expression;

    @Override
    public String toString() {
        return String.valueOf(expression);
    }
}
