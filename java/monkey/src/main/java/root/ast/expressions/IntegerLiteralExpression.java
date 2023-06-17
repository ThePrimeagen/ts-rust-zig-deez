package root.ast.expressions;

import root.Token;

public class IntegerLiteralExpression extends Expression {

    private Long value;

    public IntegerLiteralExpression(Token token, long value) {
        this.token = token;
        this.value = value;
    }

    public Long getValue() {
        return value;
    }

    public void setValue(Long value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return this.tokenLiteral();
    }
}
