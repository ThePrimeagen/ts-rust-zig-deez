package root;

import root.lexer.LexerIterable;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public class REPL {

    private static final String PROMPT = ">> ";

    private final InputStream inputStream;
    private final PrintStream printStream;

    public REPL(InputStream inputStream, PrintStream printStream) {
        this.inputStream = inputStream;
        this.printStream = printStream;
    }

    public void start() {
        var scanner = new Scanner(inputStream);

        printStream.print(PROMPT);
        while (scanner.hasNextLine()) {
            var line = scanner.nextLine();
            LexerIterable.fromString(line).forEachRemaining(this::printToken);
            printStream.print(PROMPT);
        }
    }

    public void printToken(Token token) {
        if (token.type() == TokenType.ILLEGAL) {
            throw new IllegalArgumentException("Illegal Token Provided: " + token.literal());
        }
        printStream.println(token);
    }
}
