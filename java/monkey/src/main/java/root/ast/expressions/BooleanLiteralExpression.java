package root.ast.expressions;

import root.LocalizedToken;
import root.Token;

public class BooleanLiteralExpression extends Expression {

    private final Boolean value;

    public BooleanLiteralExpression(LocalizedToken token, boolean value) {
        super(token);
        this.value = value;
    }

    public Boolean getValue() {
        return value;
    }

    @Override
    public String stringRep() {
        return tokenLiteral();
    }
}
