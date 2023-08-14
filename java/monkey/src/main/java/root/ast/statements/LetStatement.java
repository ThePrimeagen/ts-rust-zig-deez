package root.ast.statements;

import root.LocalizedToken;
import root.Token;
import root.ast.expressions.Expression;
import root.ast.expressions.IdentifierExpression;

public class LetStatement extends Statement {

    private IdentifierExpression name;
    private Expression value;

    public LetStatement(LocalizedToken token) {
        this.token = token;
    }

    public IdentifierExpression getName() {
        return name;
    }

    public void setName(IdentifierExpression name) {
        this.name = name;
    }

    public Expression getValue() {
        return value;
    }

    public void setValue(Expression value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "%s %s = %s;".formatted(tokenLiteral(), name, value);
    }
}
