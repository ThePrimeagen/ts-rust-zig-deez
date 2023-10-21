package root.evaluation.objects;

import root.LocalizedToken;
import root.evaluation.EvaluationException;

public abstract class AbstractMonkeyFunction extends MonkeyObject<MonkeyFunctionInterface> {

    public AbstractMonkeyFunction(ObjectType type) {
        super(type);
    }

    public static void checkArgumentCount(LocalizedToken callToken, int expected, int actual) throws EvaluationException {
        if (actual != expected) {
            throw new EvaluationException(callToken, "Wrong number of arguments. Expected %d, got %d", expected, actual);
        }
    }

    public static void checkArgumentType(
            LocalizedToken callToken,
            MonkeyObject<?> argument,
            ObjectType expected,
            String functionName
    ) throws EvaluationException {
        if (argument.getType() != expected) {
            throw new EvaluationException(callToken, "Argument to `%s` must be %s, got %s", functionName, expected, argument.getType());
        }
    }
}
