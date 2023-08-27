package root.ast.expressions;

import root.LocalizedToken;
import root.ast.statements.BlockStatement;

public class IfExpression extends Expression {

    private Expression condition;
    private BlockStatement consequence;
    private BlockStatement alternative;

    public IfExpression(LocalizedToken token) {
        super(token);
    }

    public Expression getCondition() {
        return condition;
    }

    public void setCondition(Expression condition) {
        this.condition = condition;
    }

    public BlockStatement getConsequence() {
        return consequence;
    }

    public void setConsequence(BlockStatement consequence) {
        this.consequence = consequence;
    }

    public BlockStatement getAlternative() {
        return alternative;
    }

    public void setAlternative(BlockStatement alternative) {
        this.alternative = alternative;
    }

    @Override
    public String stringRep() {
        var ifString = "if (%s) %s".formatted(condition.stringRep(), consequence.stringRep());

        if (alternative != null) {
            ifString += " else %s".formatted(alternative.stringRep());
        }

        return ifString;
    }
}
