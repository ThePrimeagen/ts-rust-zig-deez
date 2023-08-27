package root.ast.expressions;

import root.LocalizedToken;

public class InfixExpression extends Expression {

    private final Expression left;
    private final String operator;
    private Expression right;

    public InfixExpression(LocalizedToken token, String operator, Expression left) {
        super(token);
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
    public String stringRep() {
        return "(%s %s %s)".formatted(left.stringRep(), operator, right.stringRep());
    }
}
