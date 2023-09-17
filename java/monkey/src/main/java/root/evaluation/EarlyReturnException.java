package root.evaluation;

import root.evaluation.objects.MonkeyObject;

public class EarlyReturnException extends RuntimeException {

    public final MonkeyObject<?> returnValue;

    public EarlyReturnException(MonkeyObject<?> returnValue) {
        this.returnValue = returnValue;
    }
}
