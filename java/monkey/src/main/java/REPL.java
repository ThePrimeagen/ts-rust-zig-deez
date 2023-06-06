import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public class REPL {

    private static final String PROMPT = ">> ";

    public void start(InputStream inputStream, PrintStream printStream) {
        var scanner = new Scanner(inputStream);

        while (true) {
            printStream.print(PROMPT);
            var line = scanner.nextLine();

            if (line.trim().length() == 0) {
                break;
            }

            var lexer = new Lexer(line);

            var t = lexer.nextToken();
            while (t.type() != TokenType.EOF) {
                printStream.println(t);
                t = lexer.nextToken();
            }
        }
    }
}
