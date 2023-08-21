import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.evaluation.Evaluator;
import root.evaluation.EvaluationException;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.impl.*;
import root.lexer.Lexer;
import root.parser.ParseProgramException;
import root.parser.Parser;

import java.util.List;

public class EvaluatorTest {

    private record IntegerTest(long expected, String input) {
    }

    @Test
    void testIntegerExpression() {
        var tests = List.of(
                new IntegerTest(5, "5"),
                new IntegerTest(10, "10"),
                new IntegerTest(0, "0"),
                new IntegerTest(-5, "-5"),
                new IntegerTest(-10, "-10"),
                new IntegerTest(100, "--100"),
                new IntegerTest(10, "5 + 5 + 5 + 5 - 10"),
                new IntegerTest(32, "2 * 2 * 2 * 2 * 2"),
                new IntegerTest(0, "-50 + 100 + -50"),
                new IntegerTest(20, "5 * 2 + 10"),
                new IntegerTest(25, "5 + 2 * 10"),
                new IntegerTest(0, "20 + 2 * -10"),
                new IntegerTest(60, "50 / 2 * 2 + 10"),
                new IntegerTest(30, "2 * (5 + 10)"),
                new IntegerTest(37, "3 * 3 * 3 + 10"),
                new IntegerTest(37, "3 * (3 * 3) + 10"),
                new IntegerTest(50, "(5 + 10 * 2 + 15 / 3) * 2 + -10")
        );

        for (IntegerTest(long expected, String input) : tests) {
            testIntegerObject(expected, testEval(input));
        }
    }

    private record BooleanTest(boolean expected, String input) {
    }

    @Test
    void testBooleanExpression() {
        var tests = List.of(
                new BooleanTest(true, "true"),
                new BooleanTest(false, "false"),
                new BooleanTest(true, "1 < 2"),
                new BooleanTest(false, "1 > 2"),
                new BooleanTest(false, "1 < 1"),
                new BooleanTest(false, "1 > 1"),
                new BooleanTest(true, "1 == 1"),
                new BooleanTest(false, "1 != 1"),
                new BooleanTest(false, "1 == 2"),
                new BooleanTest(true, "1 != 2"),
                new BooleanTest(true, "true == true"),
                new BooleanTest(true, "false == false"),
                new BooleanTest(false, "true == false"),
                new BooleanTest(true, "true != false"),
                new BooleanTest(true, "false != true"),
                new BooleanTest(true, "(1 < 2) == true"),
                new BooleanTest(false, "(1 < 2) == false"),
                new BooleanTest(false, "(1 > 2) == true"),
                new BooleanTest(true, "(1 > 2) == false"),
                new BooleanTest(true, "null == null"),
                new BooleanTest(true, "!null == true"),
                new BooleanTest(false, "null != null"),
                new BooleanTest(true, "null != !null"),
                new BooleanTest(true, "null != 10"),
                new BooleanTest(true, "null != 0"),
                new BooleanTest(false, "null == false"),
                new BooleanTest(false, "null == true"),
                new BooleanTest(false, "null == 10"),
                new BooleanTest(false, "null == 0")
        );

        for (BooleanTest(boolean expected, String input) : tests) {
            testBooleanObject(expected, testEval(input));
        }
    }

    @Test
    void testBangOperator() {
        var tests = List.of(
                new BooleanTest(false, "!true"),
                new BooleanTest(true, "!false"),
                new BooleanTest(false, "!5"),
                new BooleanTest(true, "!!5"),
                new BooleanTest(false, "!!!0"),
                new BooleanTest(true, "!null")
        );

        for (BooleanTest(boolean expected, String input) : tests) {
            testBooleanObject(expected, testEval(input));
        }
    }

    private record ExpressionTest(Object expected, String input) {
    }

    @Test
    void testIfElseExpression() {
        var tests = List.of(
                new ExpressionTest(10, "if (true) { 10 }"),
                new ExpressionTest(null, "if (false) { 10 }"),
                new ExpressionTest(10, "if (1) { 10 }"),
                new ExpressionTest(10, "if (1 < 2) { 10 }"),
                new ExpressionTest(null, "if (1 > 2) { 10 }"),
                new ExpressionTest(20, "if (1 > 2) { 10 } else { 20 }"),
                new ExpressionTest(10, "if (1 < 2) { 10 } else { 20 }"),
                new ExpressionTest(10, "if (!null) { 10 } else { 20 }")
        );

        for (ExpressionTest(Object expected, String input) : tests) {
            MonkeyObject<?> evaluated = testEval(input);
            testObject(expected, evaluated);
        }
    }

    @Test
    void testReturnStatement() {
        var tests = List.of(
                new ExpressionTest(10, "return 10;"),
                new ExpressionTest(10, "return 10; 9;"),
                new ExpressionTest(10, "return 2 * 5; 9;"),
                new ExpressionTest(10, "9; return 2 * 5; 9;"),
                new ExpressionTest(MonkeyUnit.INSTANCE, "9; return; 9;"),
                new ExpressionTest(10, """
                        if (10 > 1) {
                            if (10 > 1) {
                                return 10;
                            }
                                                
                            return 1;
                        }""")
        );

        for (ExpressionTest(Object expected, String input) : tests) {
            MonkeyObject<?> evaluated = testEval(input);
            testObject(expected, evaluated);
        }
    }

    @Test
    void testErrorHandling() {
        var tests = List.of(
                List.of(
                        "5 + true;",
                        """
                                Error evaluating the program: Operation + not supported for types INTEGER and BOOLEAN
                                01: 5 + true;
                                ------^------"""
                ),
                List.of(
                        "5 + true; 5;",
                        """
                                Error evaluating the program: Operation + not supported for types INTEGER and BOOLEAN
                                01: 5 + true; 5;
                                ------^---------"""
                ),
                List.of(
                        "-true",
                        """
                                Error evaluating the program: Operation - not supported for type BOOLEAN
                                01: -true
                                ----^----"""
                ),
                List.of(
                        "true + false;",
                        """
                                Error evaluating the program: Operation + not supported for types BOOLEAN and BOOLEAN
                                01: true + false;
                                ---------^-------"""
                ),
                List.of(
                        "5; true + false; 5",
                        """
                                Error evaluating the program: Operation + not supported for types BOOLEAN and BOOLEAN
                                01: 5; true + false; 5
                                ------------^---------"""
                ),
                List.of(
                        "if (10 > 1) { true + false; }",
                        """
                                Error evaluating the program: Operation + not supported for types BOOLEAN and BOOLEAN
                                01: if (10 > 1) { true + false; }
                                -----------------------^---------"""
                ),
                List.of(
                        """
                                if (10 > 1) {
                                    if (10 > 1) {
                                        return true + false;
                                    }
                                    return 1;
                                }""",
                        """
                                Error evaluating the program: Operation + not supported for types BOOLEAN and BOOLEAN
                                03:         return true + false;
                                ------------------------^-------"""
                ),
                List.of(
                        "null < null",
                        """
                                Error evaluating the program: Null value error: both values are null
                                01: null < null
                                ---------^-----"""
                ),
                List.of(
                        "null < 5",
                        """
                                Error evaluating the program: Null value error: left value is null
                                01: null < 5
                                ---------^--"""
                ),
                List.of(
                        "5 < null",
                        """
                                Error evaluating the program: Null value error: right value is null
                                01: 5 < null
                                ------^-----"""
                ),
                List.of(
                        "null > null",
                        """
                                Error evaluating the program: Null value error: both values are null
                                01: null > null
                                ---------^-----"""
                ),
                List.of(
                        "null > 5",
                        """
                                Error evaluating the program: Null value error: left value is null
                                01: null > 5
                                ---------^--"""
                ),
                List.of(
                        "38 / 0",
                        """
                                Error evaluating the program: Cannot divide by 0
                                01: 38 / 0
                                -------^--"""
                ),
                List.of(
                        "let a = fn () { return; }()",
                        """
                                Error evaluating the program: Cannot bind unit (void) to a variable
                                01: let a = fn () { return; }()
                                ----^--------------------------"""
                ),
                List.of(
                        "let a = 5; a()",
                        """
                                Error evaluating the program: Cannot call non Function object
                                01: let a = 5; a()
                                ----------------^-"""
                )

        );

        for (var test : tests) {
            EvaluationException exception = null;
            try {
                testEval(test.get(0));
            } catch (RuntimeException e) {
                exception = (EvaluationException) e.getCause();
            }

            Assertions.assertNotNull(exception);
            Assertions.assertEquals(test.get(1), exception.getMessage());
        }
    }

    @Test
    void testNullOperations() {
        var tests = List.of(
                "null + null",
                "null + 10",
                "-5 + null",
                "null - null",
                "null - 10",
                "-5 - null",
                "null / null",
                "null / 10",
                "5 / null",
                "null * null",
                "null * 10",
                "5 * null",
                "-null",
                "fn(x) { if (x < 0) { null } else { x } }(-32)"
        );

        for (var input : tests) {
            testNullObject(testEval(input));
        }
    }

    @Test
    void testLetStatement() {
        var tests = List.of(
                new IntegerTest(5, """
                        let a = 5;
                        a;"""),
                new IntegerTest(25, """
                        let a = 5 * 5;
                        a;"""),
                new IntegerTest(10, """
                        let a = 10;
                        let b = a;
                        b"""),
                new IntegerTest(15, """
                        let a = 5;
                        let b = a;
                        let c = a + b + 5;
                        c;""")
        );

        for (IntegerTest(long expected, String input) : tests) {
            testIntegerObject(expected, testEval(input));
        }
    }

    @Test
    void testFunctionObject() {
        var input = "fn(x) { x + 2; };";

        var evaluated = testEval(input);
        var function = Assertions.assertInstanceOf(MonkeyFunction.class, evaluated);

        Assertions.assertEquals(1, function.getFunctionLiteral().getParameters().size());
        Assertions.assertEquals("x", function.getFunctionLiteral().getParameters().get(0).toString());
        Assertions.assertEquals("{\n(x + 2)\n}", function.getFunctionLiteral().getBody().toString());
    }

    @Test
    void testEvalFunction() {
        var tests = List.of(
                new IntegerTest(5, "let identity = fn(x) { x; }; identity(5);"),
                new IntegerTest(9, "let identity = fn(x) { return x; }; identity(9);"),
                new IntegerTest(10, "let double = fn(x) { x * 2; }; double(5);"),
                new IntegerTest(12, "let add = fn(x, y) { x + y; }; add(5, 7);"),
                new IntegerTest(20, "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"),
                new IntegerTest(3, "fn(x) { x; }(3);"),
                new IntegerTest(4, """
                        let newAdder = fn(x) {
                             fn(y) { x + y };
                        };
                        let addTwo = newAdder(2);
                        addTwo(2);"""),
                new IntegerTest(10, """
                        let a = 10;
                        let add_to_a = fn () {
                            let a = a + 10;
                        }
                        add_to_a();
                        a"""),
                new IntegerTest(55, """
                        let fibonacci = fn(x) { if (x < 1) { return 0 } if (x == 1) { return 1 } return fibonacci(x - 1) + fibonacci(x - 2) }
                        fibonacci(10)""")
        );

        for (IntegerTest(long expected, String input) : tests) {
            testIntegerObject(expected, testEval(input));
        }
    }

    private MonkeyObject<?> testEval(String input) {
        var l = new Lexer(input);
        var p = new Parser(l);
        var evaluator = new Evaluator();

        try {
            return evaluator.eval(p.parseProgram());
        } catch (ParseProgramException | EvaluationException e) {
            throw new RuntimeException(e);
        }
    }

    private void testObject(Object expected, MonkeyObject<?> object) {
        switch (expected) {
            case Long l -> testIntegerObject(l, object);
            case Integer i -> testIntegerObject(i, object);
            case Boolean b -> testBooleanObject(b, object);
            case MonkeyUnit unit -> Assertions.assertEquals(object, unit);
            case null -> testNullObject(object);
            default -> throw new IllegalStateException("Unexpected value: " + expected);
        }
    }

    private void testIntegerObject(long expected, MonkeyObject<?> object) {
        var integer = Assertions.assertInstanceOf(MonkeyInteger.class, object);
        Assertions.assertEquals(expected, integer.getValue());
    }

    private void testBooleanObject(boolean expected, MonkeyObject<?> object) {
        var bool = Assertions.assertInstanceOf(MonkeyBoolean.class, object);
        Assertions.assertEquals(expected, bool.getValue());
    }

    private void testNullObject(MonkeyObject<?> object) {
        if (object != MonkeyNull.INSTANCE) {
            throw new AssertionError("Object is not MonkeyNull: " + object);
        }
    }
}
