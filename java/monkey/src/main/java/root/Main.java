package root;

import root.ast.Program;
import root.evaluation.EvaluationException;
import root.evaluation.Evaluator;
import root.evaluation.objects.MonkeyObject;
import root.lexer.Lexer;
import root.parser.ParseProgramException;
import root.parser.Parser;

import java.io.*;

public class Main {

    public static void main(String[] args) {
        // Reading args as input if they exist
        if (args.length > 0) {
            runFileOrInput(args);
            return;
        }
        String username = System.getProperty("user.name");

        System.out.printf("Hello %s! This is the Monkey programming language!%n", username);
        System.out.println("Feel free to type in commands!");

        REPL repl = new REPL(System.in, System.out, true);
        repl.start();
    }

    private static void runFileOrInput(String[] args) {
        File file;
        String input;

        if (args.length == 1 && (file = new File(args[0])).exists()) {
            try {
                var inputStream = new FileInputStream(file);
                input = new String(inputStream.readAllBytes());
                inputStream.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        } else {
            input = String.join(" ", args);
        }

        var lexer = new Lexer(input);
        var parser = new Parser(lexer);
        var evaluator = new Evaluator();

        try {
            Program program = parser.parseProgram();
            MonkeyObject<?> evaluated = evaluator.eval(program);

            System.out.println(evaluated.inspect());
        } catch (ParseProgramException | EvaluationException e) {
            System.err.println(e.getMessage());
        }
    }
}