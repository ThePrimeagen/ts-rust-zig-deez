package root.ast.expressions;

import root.LocalizedToken;

/**
 * An expression that returns null, the "null" literal. This is another deviation from the
 * Monkey spec but, if the language is going to have null values, there should be a way to
 * produce them and assign them to variables with a simple syntax like, "let a = null;". In the
 * current Monkey spec, the only way to get null is something like "let a = if (false) {}"
 * or "let a = fn(){}()" which is pretty ugly.
 * <p>
 * I also changed the way null behaves, trying to make it a little more safe: the -
 * operation on null evaluates to null, and all math operations where one side is null evaluate
 * to null. Is this a good idea? I don't know, just experimenting. For the equality operator,
 * null == null evaluates to true. All other comparisons with null return false. For comparison
 * operators, if one side is null a Null value error is generated
 */
public class NullLiteralExpression extends Expression {

    public NullLiteralExpression(LocalizedToken token) {
        super(token);
    }

    @Override
    public String stringRep() {
        return "null";
    }
}
