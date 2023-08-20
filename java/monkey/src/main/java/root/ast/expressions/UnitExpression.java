package root.ast.expressions;

/**
 * An expression that returns a Unit value (aka no value).
 * See {@link root.evaluation.objects.impl.MonkeyUnit}
 */
public class UnitExpression extends Expression {

    public static UnitExpression INSTANCE = new UnitExpression();

    private UnitExpression() {
    }

    @Override
    public String toString() {
        return "";
    }
}
