import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.TokenType;
import root.ast.Program;
import root.ast.expressions.*;
import root.ast.statements.ExpressionStatement;
import root.ast.statements.LetStatement;
import root.ast.statements.ReturnStatement;
import root.ast.statements.Statement;
import root.evaluation.objects.impl.MonkeyUnit;
import root.lexer.Lexer;
import root.parser.ParseProgramException;
import root.parser.Parser;
import root.parser.ParserException;

import java.util.List;

public class ParserTest {

    private record InputStatementTest(String input, String expectedIdentifier, Object expectedValue) {
    }

    @Test
    void testLetStatements() {
        var tests = List.of(
                new InputStatementTest("let x = 5;", "x", 5),
                new InputStatementTest("let y = true;", "y", true),
                new InputStatementTest("let foobar = y;", "foobar", "y"),
                new InputStatementTest("let nil = null;", "nil", null)
        );

        for (InputStatementTest(String input, String expectedIdentifier, Object expectedValue) : tests) {
            var program = buildProgram(input);
            Assertions.assertEquals(1, program.getStatements().size());
            var letStatement = (LetStatement) program.getStatements().get(0);

            testLetStatement(letStatement, expectedIdentifier, expectedValue);
        }
    }

    private record ReturnStatementTest(String input, Object expectedValue) {
    }

    @Test
    void testReturnStatements() {
        var tests = List.of(
                new ReturnStatementTest("return 5;", 5),
                new ReturnStatementTest("return true;", true),
                new ReturnStatementTest("return y;", "y"),
                new ReturnStatementTest("return;", MonkeyUnit.INSTANCE),
                new ReturnStatementTest("return null;", null)
        );

        for (ReturnStatementTest(String input, Object expectedValue) : tests) {
            var program = buildProgram(input);
            Assertions.assertEquals(1, program.getStatements().size());
            var letStatement = Assertions.assertInstanceOf(ReturnStatement.class, program.getStatements().get(0));

            testReturnStatement(letStatement, expectedValue);
        }
    }

    @Test
    void testString() {
        var codeLine = "let myVar = anotherVar;";
        var program = new Program() {
            {
                getStatements().add(
                        new LetStatement(TokenType.LET.token().localize(
                                0, 0, codeLine
                        )) {
                            {
                                setName(new IdentifierExpression(TokenType.IDENTIFIER.createToken("myVar").localize(
                                        0, 4, codeLine
                                ), "myVar"));
                                setValue(new IdentifierExpression(TokenType.IDENTIFIER.createToken("anotherVar").localize(
                                        0, 12, codeLine
                                ), "anotherVar"));
                            }
                        }
                );
            }
        };

        Assertions.assertEquals("let myVar = anotherVar;", program.toString());
    }

    @Test
    void TestIdentifierExpression() {
        var input = "foobar;";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());

        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        testIdentifier(statement.getExpression(), "foobar");
    }

    @Test
    void testIntegerLiteralExpression() {
        var input = "5;";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());

        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        testIntegerLiteral(statement.getExpression(), 5L);
    }

    @Test
    void testBooleanLiteralExpression() {
        var input = "true; false;";

        var program = buildProgram(input);

        Assertions.assertEquals(2, program.getStatements().size());

        var statement1 = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var statement2 = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(1));

        testBooleanLiteral(statement1.getExpression(), true);
        testBooleanLiteral(statement2.getExpression(), false);
    }

    @Test
    void testNullLiteralExpression() {
        var input = "null;";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());

        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        testNullLiteral(statement.getExpression());
    }

    @Test
    void testStringLiteralExpression() {
        var input = "\"Hello\\nworld!\"";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());

        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        testStringLiteral(statement.getExpression(), "Hello\nworld!");
    }

    private record PrefixTestRecord(String input, String operator, Object right) {
    }

    @Test
    void testParsingPrefixExpressions() {
        var prefixTests = List.of(
                new PrefixTestRecord("!5", "!", 5),
                new PrefixTestRecord("- 15;", "-", 15),
                new PrefixTestRecord("!true", "!", true),
                new PrefixTestRecord("!false", "!", false),
                new PrefixTestRecord("!null", "!", null)
        );

        for (PrefixTestRecord(String input, String operator, Object right) : prefixTests) {
            Program program = buildProgram(input);

            Assertions.assertEquals(1, program.getStatements().size());

            var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

            testPrefixExpression(statement.getExpression(), operator, right);
        }
    }

    private record InfixTestRecord(String input, Object leftValue, String operator, Object rightValue) {
    }

    @Test
    void testParsingInfixExpressions() {
        var infixTests = List.of(
                new InfixTestRecord("5 + 5;", 5, "+", 5),
                new InfixTestRecord("3 + 10", 3, "+", 10),
                new InfixTestRecord("5 - 5;", 5, "-", 5),
                new InfixTestRecord("5 * 5;", 5, "*", 5),
                new InfixTestRecord("5 / 5;", 5, "/", 5),
                new InfixTestRecord("5 > 5;", 5, ">", 5),
                new InfixTestRecord("5 < 5;", 5, "<", 5),
                new InfixTestRecord("0 < 83", 0, "<", 83),
                new InfixTestRecord("5 == 5;", 5, "==", 5),
                new InfixTestRecord("5 != 5;", 5, "!=", 5),
                new InfixTestRecord("true == true", true, "==", true),
                new InfixTestRecord("true != false;", true, "!=", false),
                new InfixTestRecord("false == false", false, "==", false)
        );

        for (InfixTestRecord(String input, Object leftValue, String operator, Object rightValue) : infixTests) {
            var program = buildProgram(input);

            Assertions.assertEquals(1, program.getStatements().size());

            var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

            testInfixExpression(statement.getExpression(), leftValue, operator, rightValue);
        }
    }

    @Test
    void testOperatorPrecedenceParsing() {
        var tests = List.of(
                List.of(
                        "-a * b",
                        "((-a) * b)"
                ),
                List.of(
                        "!-a",
                        "(!(-a))"
                ),
                List.of(
                        "a + b + c",
                        "((a + b) + c)"
                ),
                List.of(
                        "a + b - c",
                        "((a + b) - c)"
                ),
                List.of(
                        "a * b * c",
                        "((a * b) * c)"
                ),
                List.of(
                        "a * b / c",
                        "((a * b) / c)"
                ),
                List.of(
                        "a + b / c",
                        "(a + (b / c))"
                ),
                List.of(
                        "a + b * c + d / e - f",
                        "(((a + (b * c)) + (d / e)) - f)"
                ),
                List.of(
                        "3 + 4; -5 * 5",
                        "(3 + 4)\n((-5) * 5)"
                ),
                List.of(
                        "5 > 4 == 3 < 4",
                        "((5 > 4) == (3 < 4))"
                ),
                List.of(
                        "5 < 4 != 3 > 4",
                        "((5 < 4) != (3 > 4))"
                ),
                List.of(
                        "3 + 4 * 5 == 3 * 1 + 4 * 5",
                        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
                ),
                List.of(
                        "3 + 4 * 5 == 3 * 1 + 4 * 5",
                        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
                ),
                List.of(
                        "true",
                        "true"
                ),
                List.of(
                        "false",
                        "false"
                ),
                List.of(
                        "3 > 5 == false",
                        "((3 > 5) == false)"
                ),
                List.of(
                        "3 < 5 == true",
                        "((3 < 5) == true)"
                ),
                List.of(
                        "1 + (2 + 3) + 4",
                        "((1 + (2 + 3)) + 4)"
                ),
                List.of(
                        "(5 + 5) * 2",
                        "((5 + 5) * 2)"
                ),
                List.of(
                        "2 / (5 + 5)",
                        "(2 / (5 + 5))"
                ),
                List.of(
                        "-(5 + 5)",
                        "(-(5 + 5))"
                ),
                List.of(
                        "!(true == true)",
                        "(!(true == true))"
                ),
                List.of(
                        "a + add(b * c) + d",
                        "((a + add((b * c))) + d)"
                ),
                List.of(
                        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
                ),
                List.of(
                        "add(a + b + c * d / f + g)",
                        "add((((a + b) + ((c * d) / f)) + g))"
                ),
                List.of(
                        "a * [1, 2, 3, 4][b * c] * d",
                        "((a * ([1, 2, 3, 4][(b * c)])) * d)"
                ),
                List.of(
                        "add(a * b[2], b[1], 2 * [1, 2][1])",
                        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"
                )
        );

        for (List<String> test : tests) {
            var program = buildProgram(test.get(0));

            Assertions.assertEquals(test.get(1), program.toString());
        }
    }

    @Test
    void testIfExpression() {
        var input = "if (x < y) { x }";
        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());
        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        var expression = Assertions.assertInstanceOf(IfExpression.class, statement.getExpression());
        testInfixExpression(expression.getCondition(), "x", "<", "y");

        Assertions.assertEquals(1, expression.getConsequence().getStatements().size());
        var consequence = Assertions.assertInstanceOf(ExpressionStatement.class, expression.getConsequence().getStatements().get(0));
        testIdentifier(consequence.getExpression(), "x");

        Assertions.assertNull(expression.getAlternative());

        Assertions.assertEquals("if ((x < y)) {\nx\n}", expression.toString());

        input = "if (y > x) { x } else { y }";
        program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());
        statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        expression = Assertions.assertInstanceOf(IfExpression.class, statement.getExpression());
        testInfixExpression(expression.getCondition(), "y", ">", "x");

        Assertions.assertEquals(1, expression.getConsequence().getStatements().size());
        consequence = Assertions.assertInstanceOf(ExpressionStatement.class, expression.getConsequence().getStatements().get(0));
        testIdentifier(consequence.getExpression(), "x");

        Assertions.assertNotNull(expression.getAlternative());
        Assertions.assertEquals(1, expression.getAlternative().getStatements().size());
        var alternative = Assertions.assertInstanceOf(ExpressionStatement.class, expression.getAlternative().getStatements().get(0));
        testIdentifier(alternative.getExpression(), "y");

        Assertions.assertEquals("if ((y > x)) {\nx\n} else {\ny\n}", expression.toString());

        input = "if (true) { x }";
        program = buildProgram(input);
        Assertions.assertEquals("if (true) {\nx\n}", program.toString());
    }

    @Test
    void testFunctionLiteralParsing() {
        var input = "fn(x, y) { x + y; }";
        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());
        var expressionStatement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));

        var function = Assertions.assertInstanceOf(FunctionLiteralExpression.class, expressionStatement.getExpression());

        Assertions.assertEquals(2, function.getParameters().size());
        testLiteralExpression(function.getParameters().get(0), "x");
        testLiteralExpression(function.getParameters().get(1), "y");

        Assertions.assertEquals(1, function.getBody().getStatements().size());

        var expression = Assertions.assertInstanceOf(ExpressionStatement.class, function.getBody().getStatements().get(0));

        testInfixExpression(expression.getExpression(), "x", "+", "y");

        Assertions.assertEquals("fn(x, y) {\n(x + y)\n}", function.toString());
    }

    private record FunctionParameterTest(String input, List<String> expectedParams) {
    }

    @Test
    void testFunctionParameterParsing() {
        var tests = List.of(
                new FunctionParameterTest("fn () {};", List.of()),
                new FunctionParameterTest("fn (x) {};", List.of("x")),
                new FunctionParameterTest("fn (x, y, z) {};", List.of("x", "y", "z"))
        );

        for (FunctionParameterTest(String input, List<String> expectedParams) : tests) {
            var program = buildProgram(input);
            Assertions.assertEquals(1, program.getStatements().size());
            var function = (FunctionLiteralExpression) ((ExpressionStatement) program.getStatements().get(0)).getExpression();

            Assertions.assertEquals(expectedParams.size(), function.getParameters().size());

            for (int i = 0; i < expectedParams.size(); i++) {
                testLiteralExpression(function.getParameters().get(i), expectedParams.get(i));
            }
        }
    }

    @Test
    void testCallExpression() {
        var input = "add(1, 2 * 3, 4 + 5)";
        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());
        var statement = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var callExpression = Assertions.assertInstanceOf(CallExpression.class, statement.getExpression());

        testIdentifier(callExpression.getFunction(), "add");
        Assertions.assertEquals(3, callExpression.getArguments().size());
        testLiteralExpression(callExpression.getArguments().get(0), 1);
        testInfixExpression(callExpression.getArguments().get(1), 2, "*", 3);
        testInfixExpression(callExpression.getArguments().get(2), 4, "+", 5);
    }

    private record CallParameterTest(String input, List<String> expectedArguments) {
    }

    @Test
    void testCallParameterParsing() {
        var tests = List.of(
                new CallParameterTest("func()", List.of()),
                new CallParameterTest("func(3 - 1)", List.of("(3 - 1)")),
                new CallParameterTest("func(1, 2 * 3 + 1, 98)", List.of("1", "((2 * 3) + 1)", "98"))
        );

        for (CallParameterTest(String input, List<String> expectedArguments) : tests) {
            var program = buildProgram(input);
            Assertions.assertEquals(1, program.getStatements().size());
            var call = (CallExpression) ((ExpressionStatement) program.getStatements().get(0)).getExpression();

            Assertions.assertEquals(expectedArguments.size(), call.getArguments().size());

            for (int i = 0; i < expectedArguments.size(); i++) {
                Assertions.assertEquals(expectedArguments.get(i), call.getArguments().get(i).toString());
            }
        }
    }

    @Test
    void testErrorMessages() {
        var input = """
                let foo = add(a,b
                """;

        var l = new Lexer(input);
        var p = new Parser(l);
        List<ParserException> errors = null;

        try {
            p.parseProgram();
        } catch (ParseProgramException e) {
            errors = e.getParseErrors();
        }

        Assertions.assertNotNull(errors);
        Assertions.assertEquals(1, errors.size());
        Assertions.assertEquals(
                """
                        InvalidSyntax: Expected next token to be ), got eof
                        01: let foo = add(a,b
                        --------------------^""",
                errors.get(0).getMessage()
        );
        Assertions.assertEquals("b", errors.get(0).getLocalizedToken().literal());
    }

    @Test
    void testParsingArrayLiterals() {
        var input = "[1, 2 + 5, 6 * 2];\n" +
                    "[];";

        var program = buildProgram(input);

        Assertions.assertEquals(2, program.getStatements().size());
        var expression = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var array = Assertions.assertInstanceOf(ArrayLiteralExpression.class, expression.getExpression());

        Assertions.assertEquals(3, array.getElements().size());
        testIntegerLiteral(array.getElements().get(0), 1L);
        testInfixExpression(array.getElements().get(1), 2, "+", 5);
        testInfixExpression(array.getElements().get(2), 6, "*", 2);
        Assertions.assertEquals("[1, (2 + 5), (6 * 2)]", array.toString());

        expression = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(1));
        array = Assertions.assertInstanceOf(ArrayLiteralExpression.class, expression.getExpression());

        Assertions.assertEquals(0, array.getElements().size());
        Assertions.assertEquals("[]", array.toString());
    }

    @Test
    void testParsingIndexExpressions() {
        var input = "myArray[1 + 1]";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());
        var expression = Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var index = Assertions.assertInstanceOf(IndexExpression.class, expression.getExpression());

        testIdentifier(index.getLeft(), "myArray");
        testInfixExpression(index.getIndex(), 1, "+", 1);
        Assertions.assertEquals("(myArray[(1 + 1)])", index.toString());
    }

    private void testIntegerLiteral(Expression expression, Long expectedValue) {
        if (expression instanceof IntegerLiteralExpression integerLiteralExpression) {
            Assertions.assertEquals(expectedValue, integerLiteralExpression.getValue());
            Assertions.assertEquals(expectedValue.toString(), integerLiteralExpression.tokenLiteral());
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of IntegerLiteralExpression");
        }
    }

    private void testIdentifier(Expression expression, String value) {
        if (expression instanceof IdentifierExpression identifierExpression) {
            Assertions.assertEquals(value, identifierExpression.getValue());
            Assertions.assertEquals(value, identifierExpression.tokenLiteral());
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of IdentifierExpression");
        }
    }

    private void testBooleanLiteral(Expression expression, Boolean value) {
        if (expression instanceof BooleanLiteralExpression booleanLiteralExpression) {
            Assertions.assertEquals(value, booleanLiteralExpression.getValue());
            Assertions.assertEquals(value.toString(), booleanLiteralExpression.tokenLiteral());
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of IdentifierExpression");
        }
    }

    private void testNullLiteral(Expression expression) {
        Assertions.assertInstanceOf(NullLiteralExpression.class, expression);
    }

    private void testStringLiteral(Expression expression, String value) {
        var stringLiteralExpression = Assertions.assertInstanceOf(StringLiteralExpression.class, expression);
        Assertions.assertEquals('"' + value + '"', stringLiteralExpression.toString());
        Assertions.assertEquals(value, stringLiteralExpression.tokenLiteral());
    }

    private void testUnitExpression(Expression expression) {
        Assertions.assertInstanceOf(UnitExpression.class, expression);
    }

    private void testLiteralExpression(Expression expression, Object expected) {
        switch (expected) {
            case Integer i -> testIntegerLiteral(expression, i.longValue());
            case Long i -> testIntegerLiteral(expression, i);
            case String s -> testIdentifier(expression, s);
            case Boolean b -> testBooleanLiteral(expression, b);
            case MonkeyUnit ignored -> testUnitExpression(expression);
            case null -> testNullLiteral(expression);
            default -> throw new AssertionError("Type of exp not handled. got=" + expected.getClass().getSimpleName());
        }
    }

    private void testInfixExpression(Expression expression, Object left, String operator, Object right) {
        if (expression instanceof InfixExpression infixExpression) {
            testLiteralExpression(infixExpression.getLeft(), left);
            Assertions.assertEquals(operator, infixExpression.getOperator());
            testLiteralExpression(infixExpression.getRight(), right);
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of infixExpression");
        }
    }

    private void testPrefixExpression(Expression expression, String operator, Object right) {
        if (expression instanceof PrefixExpression prefixExpression) {
            Assertions.assertEquals(operator, prefixExpression.getOperator());
            testLiteralExpression(prefixExpression.getRight(), right);
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of prefixExpression");
        }
    }

    private Program buildProgram(String input) {
        var l = new Lexer(input);
        var p = new Parser(l);

        try {
            return p.parseProgram();
        } catch (ParseProgramException e) {
            throw new AssertionError("Parser encountered errors:\n" + e.getMessage());
        }
    }

    private void testLetStatement(Statement statement, String name, Object expectedValue) {
        Assertions.assertEquals(TokenType.LET.token().literal(), statement.tokenLiteral());

        if (statement instanceof LetStatement letStatement) {
            Assertions.assertEquals(name, letStatement.getName().getValue());
            Assertions.assertEquals(name, letStatement.getName().tokenLiteral());
            testLiteralExpression(letStatement.getValue(), expectedValue);
        } else {
            throw new AssertionError(statement.getClass().getSimpleName() + " is not instance of LetStatement");
        }
    }

    private void testReturnStatement(Statement statement, Object expectedValue) {
        Assertions.assertEquals(TokenType.RETURN.token().literal(), statement.tokenLiteral());

        if (statement instanceof ReturnStatement returnStatement) {
            testLiteralExpression(returnStatement.getReturnValue(), expectedValue);
        } else {
            throw new AssertionError(statement.getClass().getSimpleName() + " is not instance of ReturnStatement");
        }
    }
}
