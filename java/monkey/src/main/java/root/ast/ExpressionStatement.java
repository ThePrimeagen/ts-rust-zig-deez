package root.ast;

public class ExpressionStatement extends Statement {

    public Expression expression;

    @Override
    public String toString() {
        return String.valueOf(expression);
    }
}
