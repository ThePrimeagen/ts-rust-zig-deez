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

    @SuppressWarnings("unchecked") // It is checked
    public static <T extends MonkeyObject<?>> T checkArgumentType(
            LocalizedToken callToken,
            MonkeyObject<?> argument,
            Class<T> expected,
            String functionName
    ) throws EvaluationException {
        if (expected.isAssignableFrom(argument.getClass())) {
            return (T) argument;
        }

        ObjectType expectedType = ObjectType.getTypeFromClass(expected);

        throw new EvaluationException(callToken, "Argument to `%s` must be %s, got %s", functionName, expectedType, argument.getType());
    }
}
