package root.evaluation.objects;

import root.LocalizedToken;
import root.evaluation.EvaluationException;

public abstract class AbstractMonkeyFunction extends MonkeyObject<MonkeyFunctionInterface> {

    public AbstractMonkeyFunction(ObjectType type) {
        super(type);
    }

    public static void throwWorngNumberOfArgumentsError(LocalizedToken callToken, int expected, int actual) throws EvaluationException {
        throw new EvaluationException(callToken, "Wrong number of arguments. Expected %d, got %d", expected, actual);
    }
}
