package root.ast.expressions;

import root.Token;

public class IdentiferExpression extends Expression {

    public String value;

    public IdentiferExpression(Token token, String value) {
        this.token = token;
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }
}
