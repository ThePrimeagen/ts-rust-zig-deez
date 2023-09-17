package root.ast.expressions;

import root.LocalizedToken;

public class StringLiteralExpression extends Expression {

    public StringLiteralExpression(LocalizedToken token) {
        super(token);
    }

    @Override
    public String stringRep() {
        return "\"%s\"".formatted(getToken().literal());
    }
}
