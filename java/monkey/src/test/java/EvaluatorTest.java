import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.evaluation.Evaluator;
import root.evaluation.EvaluationException;
import root.evaluation.objects.MonkeyObject;
import root.evaluation.objects.ObjectType;
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

        for (var test : tests) {
            testIntegerObject(test.expected, testEval(test.input));
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
                new BooleanTest(false, "null == 0"),
                new BooleanTest(true, "true && true"),
                new BooleanTest(false, "true && false"),
                new BooleanTest(false, "false && true"),
                new BooleanTest(false, "false && false"),
                new BooleanTest(true, "true || true"),
                new BooleanTest(true, "true || false"),
                new BooleanTest(true, "false || true"),
                new BooleanTest(false, "false || false"),
                new BooleanTest(true, "false && false || true"),
                new BooleanTest(false, "false && (false || true)"),
                new BooleanTest(true, "10 < 2 || 10 + 2 == 12"),
                new BooleanTest(false, "10 < 2 && 10 + 2 == 12"),
                new BooleanTest(true, "null || 10"),
                new BooleanTest(false, "'hi' && null"),
                new BooleanTest(true, "let double = fn (n) { 2 * n }; double(10) == 20 || false"),
                new BooleanTest(true, "let arr = [1, 2, 3]; arr[1] * 2 == 4 || false"),
                new BooleanTest(false, "let arr = [1, 2, 3]; let i = null; i && arr[i] == 2"),
                new BooleanTest(true, "true || 0 / 0")
        );

        for (var test : tests) {
            testBooleanObject(test.expected, testEval(test.input));
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

        for (var test : tests) {
            testBooleanObject(test.expected, testEval(test.input));
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

        for (var test : tests) {
            MonkeyObject<?> evaluated = testEval(test.input);
            testObject(test.expected, evaluated);
        }
    }

    @Test
    void testReturnStatement() {
        var tests = List.of(
                new ExpressionTest(10, "return 10;"),
                new ExpressionTest(10, "return 10; 9;"),
                new ExpressionTest(10, "return 2 * 5; 9;"),
                new ExpressionTest(10, "9; return 2 * 5; 9;"),
                new ExpressionTest(10, """
                        if (10 > 1) {
                            if (10 > 1) {
                                return 10;
                            }
                                                
                            return 1;
                        }""")
        );

        for (var test : tests) {
            MonkeyObject<?> evaluated = testEval(test.input);
            testObject(test.expected, evaluated);
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
                        "let a = 5; a()",
                        """
                                Error evaluating the program: Cannot call non Function object
                                01: let a = 5; a()
                                ----------------^-"""
                ),
                List.of(
                        "let a = fn() { 10 }; a(90)",
                        """
                                Error evaluating the program: Wrong number of arguments. Expected 0, got 1
                                01: let a = fn() { 10 }; a(90)
                                --------------------------^---"""
                ),
                List.of(
                        "fn () { put(8) }()",
                        """
                                Error evaluating the program: Variable put is not declared
                                01: fn () { put(8) }()
                                ------------^---------"""
                ),
                List.of(
                        "\"hello\" - \"world\"",
                        """
                                Error evaluating the program: Operation - not supported between Strings
                                01: "hello" - "world"
                                ------------^--------"""
                ),
                List.of(
                        "[1, 2][\"hello\"]",
                        """
                                Error evaluating the program: Index to an array must be an Expression that yields an Int
                                01: [1, 2]["hello"]
                                -----------^-------"""
                ),
                List.of(
                        "{ [1, 2]: 3 }",
                        """
                                Error evaluating the program: Index to an hash must be an Expression that yields an Int, String or Boolean
                                01: { [1, 2]: 3 }
                                ------^----------"""
                ),
                List.of(
                        "{ fn(x) { x }: 'func' }",
                        """
                                Error evaluating the program: Index to an hash must be an Expression that yields an Int, String or Boolean
                                01: { fn(x) { x }: 'func' }
                                ------^--------------------"""
                ),
                List.of(
                        "push (10, 10)",
                        """
                                Error evaluating the program: Argument to `push` must be ARRAY, got INTEGER
                                01: push (10, 10)
                                ---------^-------"""
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

        for (var test : tests) {
            testIntegerObject(test.expected, testEval(test.input));
        }
    }

    @Test
    void testFunctionObject() {
        var input = "fn(x) { x + 2; };";

        var evaluated = testEval(input);
        var function = Assertions.assertInstanceOf(MonkeyFunction.class, evaluated);

        Assertions.assertEquals(1, function.getFunctionLiteral().getParameters().size());
        Assertions.assertEquals("x", function.getFunctionLiteral().getParameters().get(0).stringRep());
        Assertions.assertEquals("{\n(x + 2)\n}", function.getFunctionLiteral().getBody().stringRep());
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

        for (var test : tests) {
            testIntegerObject(test.expected, testEval(test.input));
        }
    }

    @Test
    void testBuiltinFunctions() {
        var tests = List.of(
                new ExpressionTest(null, "puts(10)"),
                new ExpressionTest(null, "fn () { putsNoln(8) }()"),
                new ExpressionTest(MonkeyInteger.class, "let time = miliTime(); time"),
                new ExpressionTest(3, "len(\"abc\")"),
                new ExpressionTest(0, "len(\"\")"),
                new ExpressionTest(4, "len([1, 2, 3, 4])"),
                new ExpressionTest(0, "len([])"),
                new ExpressionTest(7, "len(\"abc\" + \" \" + \"def\")"),
                new ExpressionTest(0, "len({})"),
                new ExpressionTest(2, "len({1: 10, 2: 20})"),
                new ExpressionTest(3, "len(set({1: 10, 2: 20}, 3, 30))"),
                new ExpressionTest(2, "len(remove({1: 10, 2: 20}, 3))"),
                new ExpressionTest(1, "len(remove({1: 10, 2: 20}, 2))"),
                new ExpressionTest("HELLO", "uppercase(\"hello\")"),
                new ExpressionTest("HI, THIS IS A NUMBER: 42", "uppercase(\"Hi, this is a number: \" + 42)"),
                new ExpressionTest("world", "lowercase(\"WORLD\")"),
                new ExpressionTest("testing escapes: \n\t\"", "lowercase(\"Testing Escapes: \\n\\t\\\"\")"),
                new ExpressionTest(1, "first([1, 2, 3, 4])"),
                new ExpressionTest(null, "first([])"),
                new ExpressionTest(49, "last([10 * 8, 7 * 7])"),
                new ExpressionTest(List.of(4, 9), "rest([1 * 1, 2 * 2, 3 * 3])"),
                new ExpressionTest(2, "len(rest([1 * 1, 2 * 2, 3 * 3]))"),
                new ExpressionTest(List.of(), "rest([20])"),
                new ExpressionTest(null, "rest(rest(rest(rest([1 * 1, 2 * 2, 3 * 3]))))"),
                new ExpressionTest(null, "rest([])"),
                new ExpressionTest(List.of("test"), "push([], 'test')"),
                new ExpressionTest(List.of("test", 28), "push(push([], 'test'), 28)"),
                new ExpressionTest(List.of(), "let arr = []; push(arr, 10); arr"),
                new ExpressionTest(List.of("h", "e", "l", "l", "o", "!"), "chars('hello!')"),
                new ExpressionTest(List.of(), "chars('')"),
                new ExpressionTest(ObjectType.INTEGER.name(), "typeof(2 * 5)"),
                new ExpressionTest(ObjectType.ARRAY.name(), "typeof([])"),
                new ExpressionTest(ObjectType.STRING.name(), "typeof('test')"),
                new ExpressionTest(ObjectType.FUNCTION.name(), "let func = fn (x) { x * x}; typeof(func)"),
                new ExpressionTest(ObjectType.NULL.name(), "typeof(first([]))"),
                new ExpressionTest(ObjectType.NULL.name(), "typeof(puts(10 * 7))"),
                new ExpressionTest(ObjectType.HASH.name(), "typeof({'foo': 'bar'})"),
                new ExpressionTest(List.of("foo", "bar"), "asList({'foo': 'bar'})"),
                new ExpressionTest(List.of(1, 400), "let arr = [1, 2]; let arr = set(arr, 1, 20 * 20); arr"),
                new ExpressionTest(List.of(1, 2), "let arr = [1, 2]; set(arr, 1, 20 * 20); arr"),
                new ExpressionTest(List.of(1, 2), "let arr = [1, 2]; remove(arr, 1); arr"),
                new ExpressionTest(List.of(2), "let arr = [1, 2]; let arr = remove(arr, 0); arr"),
                new ExpressionTest(400, "let obj = { 10: 10 * 10 }; let obj = set(obj, 20, 20 * 20); obj[20]"),
                new ExpressionTest(null, "let obj = { 10: 10 * 10 }; set(obj, 20, 20 * 20); obj[20]"),
                new ExpressionTest(100, "let obj = { 10: 10 * 10 }; remove(obj, 10); obj[10]"),
                new ExpressionTest(null, "let obj = { 10: 10 * 10 }; let obj = remove(obj, 10); obj[10]")
        );

        for (var test : tests) {
            MonkeyObject<?> evaluated = testEval(test.input);
            testObject(test.expected, evaluated);
        }
    }

    @Test
    void testStringExpressions() {
        var tests = List.of(
                new ExpressionTest("hello world!", "let a = \"hello world!\""),
                new ExpressionTest("ana bob", "\"ana\" + \" \" + \"bob\""),
                new ExpressionTest("the value of b is: 10", "let b = 2*5; \"the value of b is: \" + b"),
                new ExpressionTest(true, "\"bob\" > \"ana\""),
                new ExpressionTest(true, "\"aaron\" < \"sharon\""),
                new ExpressionTest(true, "\"Hello world\" == \"Hello \" + \"world\""),
                new ExpressionTest(true, "\"Hello world\" != \"hello \" + \"world\"")
        );

        for (var test : tests) {
            MonkeyObject<?> evaluated = testEval(test.input);
            testObject(test.expected, evaluated);
        }
    }

    @Test
    void testArrayLiterals() {
        var input = "[1, 2 * 2, 3 + 3]";

        MonkeyObject<?> result = testEval(input);
        var array = Assertions.assertInstanceOf(MonkeyArray.class, result);
        Assertions.assertEquals(3, array.getValue().size());
        testIntegerObject(1, array.getValue().get(0));
        testIntegerObject(4, array.getValue().get(1));
        testIntegerObject(6, array.getValue().get(2));
    }

    @Test
    void testArrayIndexExpression() {
        var tests = List.of(
                new ExpressionTest(1, "[1, 2, 3][0]"),
                new ExpressionTest(2, "[1, 2, 3][1]"),
                new ExpressionTest(3, "[1, 2, 3][2]"),
                new ExpressionTest(null, "[1, 2, 3][3]"),
                new ExpressionTest(null, "[1, 2, 3][-1]"),
                new ExpressionTest(1, "let i = 0; [1][i]"),
                new ExpressionTest(3, "[1, 2, 3][1 + 1]"),
                new ExpressionTest(6, "[1, 2, 3, 4, 5][2 * 2] + 1"),
                new ExpressionTest(2, "let myArr = [1, 2, 3]; myArr[1]"),
                new ExpressionTest(6, "let myArr = [1, 2, 3]; myArr[0] + myArr[1] + myArr[2]"),
                new ExpressionTest(2, "let myArr = [1, 2, 3]; let i = myArr[0]; myArr[i]"),
                new ExpressionTest(10, """
                        let myArr = [1, 2, 3, 4];
                        let sum = fn (arr, acc, i) {
                            if (i < 0) {
                                return acc;
                            }
                            let acc = acc + arr[i];
                            return sum(arr, acc, i - 1);
                        }
                        sum(myArr, 0, 3);""")
        );

        for (var test : tests) {
            testObject(test.expected, testEval(test.input));
        }
    }

    @Test
    void testObjectLiterals() {
        var tests = List.of(
                new ExpressionTest("foo", "let obj = { 'bar': 'foo' }; obj['bar']"),
                new ExpressionTest(10, "let obj = { 'ten': 10, 'foo': 'bar' }; obj['t' + 'e' + 'n']"),
                new ExpressionTest(100, "let obj = { 10: 10 * 10 }; obj[fn(x){x*5}(2)]"),
                new ExpressionTest(2, "let obj = { 'list': [1, 2, 3] }; obj['list'][1]"),
                new ExpressionTest("falso", "let obj = { true: 'verdadeiro', false: 'falso' }; obj[10 < 2]"),
                new ExpressionTest(6, """
                        let obj = {
                            'abs': fn (a) { if (a > 0) { a } else { -a } },
                            'square': fn (a) { a * a }
                        };
                        let abs = 'abs';
                        let square = 'square';
                        obj[abs](-2) + obj[square](2)""")
        );

        for (var test : tests) {
            testObject(test.expected, testEval(test.input));
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
            case String s -> testStringObject(s, object);
            case Class<?> clazz -> Assertions.assertInstanceOf(clazz, object);
            case List<?> list -> testArrayObject(list, object);
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

    private void testStringObject(String expected, MonkeyObject<?> object) {
        var string = Assertions.assertInstanceOf(MonkeyString.class, object);
        Assertions.assertEquals(expected, string.getValue());
    }

    private void testArrayObject(List<?> expected, MonkeyObject<?> object) {
        var array = Assertions.assertInstanceOf(MonkeyArray.class, object);
        Assertions.assertEquals(expected.size(), array.getValue().size());

        for (int i = 0; i < expected.size(); i++) {
            testObject(expected.get(i), array.getValue().get(i));
        }
    }

    private void testNullObject(MonkeyObject<?> object) {
        if (object != MonkeyNull.INSTANCE) {
            throw new AssertionError("Object is not MonkeyNull: " + object);
        }
    }
}
