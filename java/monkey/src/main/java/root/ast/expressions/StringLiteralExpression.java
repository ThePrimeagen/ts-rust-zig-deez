package root.ast.expressions;

import root.LocalizedToken;

public class StringLiteralExpression extends Expression {

    public StringLiteralExpression(LocalizedToken token) {
        this.token = token;
    }

    @Override
    public String toString() {
        return token.literal();
    }
}
