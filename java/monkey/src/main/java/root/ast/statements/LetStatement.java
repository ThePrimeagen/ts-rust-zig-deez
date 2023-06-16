package root.ast.statements;

import root.Token;
import root.ast.expressions.Expression;
import root.ast.expressions.IdentiferExpression;

public class LetStatement extends Statement {

    private IdentiferExpression name;
    private Expression value;

    public LetStatement(Token token) {
        this.token = token;
    }

    public IdentiferExpression getName() {
        return name;
    }

    public void setName(IdentiferExpression name) {
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
