package root;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

public class Main {

    public static void main(String[] args) {
        InputStream inputStream;
        boolean printPrompt;

        // Reading args as input if they exist
        if (args.length > 0) {
            inputStream = new ByteArrayInputStream(String.join(" ", args).getBytes());
            printPrompt = false;
        } else {
            String username = System.getProperty("user.name");

            System.out.printf("Hello %s! This is the Monkey programming language!%n", username);
            System.out.println("Feel free to type in commands!");

            inputStream = System.in;
            printPrompt = true;
        }

        REPL repl = new REPL(inputStream, System.out, printPrompt);
        repl.start();
    }
}