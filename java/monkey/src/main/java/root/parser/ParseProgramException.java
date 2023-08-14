package root.parser;

import java.util.List;
import java.util.stream.Collectors;

public class ParseProgramException extends Exception {

    private final List<ParserException> parseErrors;

    public ParseProgramException(List<ParserException> parseErrors) {
        super("Program did not parse correctly. Errors:\n" + parseErrors
                .stream()
                .map(Throwable::getMessage)
                .collect(Collectors.joining("\n")));
        this.parseErrors = parseErrors;
    }

    public List<ParserException> getParseErrors() {
        return parseErrors;
    }
}
