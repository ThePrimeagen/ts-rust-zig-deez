package root;

import root.lexer.LexerIterable;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public class REPL {

    private static final String PROMPT = ">> ";

    private final InputStream inputStream;
    private final PrintStream printStream;
    private final boolean shouldPrintPrompt;

    public REPL(InputStream inputStream, PrintStream printStream, boolean shouldPrintPrompt) {
        this.inputStream = inputStream;
        this.printStream = printStream;
        this.shouldPrintPrompt = shouldPrintPrompt;
    }

    public void start() {
        var scanner = new Scanner(inputStream);

        printPrompt();
        while (scanner.hasNextLine()) {
            var line = scanner.nextLine();
            LexerIterable.fromString(line).forEachRemaining(this::printToken);
            printPrompt();
        }
    }

    private void printPrompt() {
        if (shouldPrintPrompt) {
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
