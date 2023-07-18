package root.ast.expressions;

import root.Token;

public class BooleanLiteralExpression extends Expression {

    private final Boolean value;

    public BooleanLiteralExpression(Token token, boolean value) {
        this.token = token;
        this.value = value;
    }

    public Boolean getValue() {
        return value;
    }

    @Override
    public String toString() {
        return tokenLiteral();
    }
}
