package root.ast.expressions;

import root.LocalizedToken;

public class PrefixExpression extends Expression {

    private final String operator;

    private Expression right;

    public PrefixExpression(LocalizedToken token, String operator) {
        super(token);
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
    public String stringRep() {
        return "(%s%s)".formatted(operator, right.stringRep());
    }
}
