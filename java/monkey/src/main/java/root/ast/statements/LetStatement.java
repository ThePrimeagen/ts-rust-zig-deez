package root.ast.statements;

import root.Token;
import root.ast.expressions.Expression;
import root.ast.expressions.IdentiferExpression;

public class LetStatement extends Statement {

    public IdentiferExpression name;
    public Expression value;

    public LetStatement(Token token) {
        this.token = token;
    }

    @Override
    public String toString() {
        return "%s %s = %s;".formatted(tokenLiteral(), name, value);
    }
}
