import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.evaluation.Evaluator;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.MonkeyBoolean;
import root.evaluation.objects.impl.MonkeyInteger;
import root.lexer.Lexer;
import root.parser.ParseProgramException;
import root.parser.Parser;

import java.util.List;

public class EvaluatorTest {

    private record IntegerLiteralTest(long expected, String input) {
    }

    @Test
    void testIntegerLiterals() {
        var tests = List.of(
                new IntegerLiteralTest(5, "5"),
                new IntegerLiteralTest(10, "10"),
                new IntegerLiteralTest(0, "0"),
                new IntegerLiteralTest(-5, "-5"),
                new IntegerLiteralTest(-10, "-10"),
                new IntegerLiteralTest(100, "--100")
        );

        for (IntegerLiteralTest(long expected, String input) : tests) {
            testIntegerObject(expected, testEval(input));
        }
    }

    private record BooleanLiteralTest(boolean expected, String input) {
    }

    @Test
    void testBooleanLiterals() {
        var tests = List.of(
                new BooleanLiteralTest(true, "true"),
                new BooleanLiteralTest(false, "false")
        );

        for (BooleanLiteralTest(boolean expected, String input) : tests) {
            testBooleanObject(expected, testEval(input));
        }
    }

    @Test
    void testMinusOperator() {
        var tests = List.of(
                new BooleanLiteralTest(false, "!true"),
                new BooleanLiteralTest(true, "!false"),
                new BooleanLiteralTest(false, "!5"),
                new BooleanLiteralTest(true, "!!5"),
                new BooleanLiteralTest(true, "!!5")
        );

        for (BooleanLiteralTest(boolean expected, String input) : tests) {
            testBooleanObject(expected, testEval(input));
        }
    }

    private MonkeyObject<?> testEval(String input) {
        var l = new Lexer(input);
        var p = new Parser(l);

        try {
            return Evaluator.eval(p.parseProgram());
        } catch (ParseProgramException e) {
            throw new RuntimeException(e);
        }
    }

    private void testIntegerObject(long expected, MonkeyObject<?> object) {
        switch (object) {
            case MonkeyInteger integer -> Assertions.assertEquals(expected, integer.getValue());
            default -> throw new AssertionError("Object is not MonkeyInteger: " + object);
        }
    }

    private void testBooleanObject(boolean expected, MonkeyObject<?> object) {
        switch (object) {
            case MonkeyBoolean bool -> Assertions.assertEquals(expected, bool.getValue());
            default -> throw new AssertionError("Object is not MonkeyBoolean: " + object);
        }
    }
}
