package root.ast.expressions;

import root.LocalizedToken;
import root.Token;

public class PrefixExpression extends Expression {

    private final String operator;

    private Expression right;

    public PrefixExpression(LocalizedToken token, String operator) {
        this.token = token;
        this.operator = operator;
    }

    public String getOperator() {
        return operator;
    }

    public Expression getRight() {
        return right;
    }

    public void setRight(Expression right) {
        this.right = right;
    }

    @Override
    public String toString() {
        return "(%s%s)".formatted(operator, right);
    }
}
