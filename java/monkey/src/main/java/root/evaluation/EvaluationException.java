package root.evaluation;

import root.LocalizedToken;
import root.util.ExceptionUtil;

public class EvaluationException extends Exception {

    public EvaluationException(LocalizedToken token, String message, Object... args) {
        super(buildMessage(token, message, args));
    }

    private static String buildMessage(LocalizedToken token, String message, Object... args) {
        var formatedMessage = message.formatted(args);
        return ExceptionUtil.buildMessage("Error evaluating the program", formatedMessage, token);
    }
}
