package root.ast.expressions;

import root.LocalizedToken;

public class IndexExpression extends Expression {

    private final Expression left;
    private final Expression index;

    public IndexExpression(LocalizedToken token, Expression left, Expression index) {
        this.left = left;
        this.index = index;
        this.token = token;
    }

    public Expression getLeft() {
        return left;
    }

    public Expression getIndex() {
        return index;
    }

    @Override
    public String toString() {
        return "(%s[%s])".formatted(left, index);
    }
}
