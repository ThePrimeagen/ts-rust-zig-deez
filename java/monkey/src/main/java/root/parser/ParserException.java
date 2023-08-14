package root.parser;

import root.LocalizedToken;
import root.util.ExceptionUtil;

public class ParserException extends Exception {

    private final LocalizedToken localizedToken;

    public ParserException(String message, LocalizedToken localizedToken) {
        super(ExceptionUtil.buildMessage("InvalidSyntax", message, localizedToken));
        this.localizedToken = localizedToken;
    }

    public LocalizedToken getLocalizedToken() {
        return localizedToken;
    }
}
