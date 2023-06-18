package root.ast.expressions;

import root.Token;

public class InfixExpression extends Expression {

    private final Expression left;
    private final String operator;
    private Expression right;


    public InfixExpression(Token token, String operator, Expression left) {
        this.token = token;
        this.operator = operator;
        this.left = left;
    }

    public Expression getLeft() {
        return left;
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
        return "(%s %s %s)".formatted(left, operator, right);
    }
}
