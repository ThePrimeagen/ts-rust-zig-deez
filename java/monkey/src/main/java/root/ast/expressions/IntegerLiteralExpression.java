package root.ast.expressions;

import root.LocalizedToken;
import root.Token;

public class IntegerLiteralExpression extends Expression {

    private final Long value;

    public IntegerLiteralExpression(LocalizedToken token, long value) {
        super(token);
        this.value = value;
    }

    public Long getValue() {
        return value;
    }

    @Override
    public String stringRep() {
        return tokenLiteral();
    }
}
