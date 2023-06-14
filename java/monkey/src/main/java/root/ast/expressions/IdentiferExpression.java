package root.ast.expressions;

import root.Token;

public class IdentiferExpression extends Expression {

    private String value;

    public IdentiferExpression(Token token, String value) {
        this.token = token;
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }
}
