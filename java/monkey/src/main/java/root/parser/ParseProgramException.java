package root.parser;

import java.util.List;

public class ParseProgramException extends Exception {

    private final List<String> parseErrors;

    public ParseProgramException(List<String> parseErrors) {
        super("Program did not parse correctly. Errors:\n" + String.join("\n", parseErrors));
        this.parseErrors = parseErrors;
    }

    public List<String> getParseErrors() {
        return parseErrors;
    }
}
