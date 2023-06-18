import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.ast.expressions.*;
import root.ast.statements.ExpressionStatement;
import root.lexer.Lexer;
import root.parser.Parser;
import root.TokenType;
import root.ast.*;
import root.ast.statements.LetStatement;
import root.ast.statements.ReturnStatement;
import root.ast.statements.Statement;

import java.util.List;

public class ParserTest {

    @Test
    void testLetStatements() {
        var input = """
                let x = 5;
                let y = 10;
                let foobar = 838383;""";

        var program = buildProgram(input);

        Assertions.assertEquals(3, program.getStatements().size());

        var expectedIdentifiers = List.of("x", "y", "foobar");

        var index = 0;
        for (var statement : program.getStatements()) {
            testLetStatement(statement, expectedIdentifiers.get(index++));
        }
    }

    @Test
    void testReturnStatements() {
        var input = """
                return 5;
                return 10;
                return 993322;""";

        var program = buildProgram(input);

        Assertions.assertEquals(3, program.getStatements().size());

        for (var statement : program.getStatements()) {
            Assertions.assertTrue(statement instanceof ReturnStatement);
            Assertions.assertEquals(TokenType.RETURN.token().literal(), statement.tokenLiteral());
        }
    }

    @Test
    void testString() {
        var program = new Program() {
            {
                getStatements().add(
                        new LetStatement(TokenType.LET.token()) {
                            {
                                setName(new IdentifierExpression(TokenType.IDENT.createToken("myVar"), "myVar"));
                                setValue(new IdentifierExpression(TokenType.IDENT.createToken("anotherVar"), "anotherVar"));
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

        Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var statement = (ExpressionStatement) program.getStatements().get(0);

        Assertions.assertInstanceOf(IdentifierExpression.class, statement.getExpression());
        var ident = (IdentifierExpression) statement.getExpression();

        var value = ident.getValue();
        Assertions.assertEquals("foobar", value);
        Assertions.assertEquals("foobar", ident.tokenLiteral());
    }

    @Test
    void testIntegerLiteralExpression() {
        var input = "5;";

        var program = buildProgram(input);

        Assertions.assertEquals(1, program.getStatements().size());

        Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
        var statement = (ExpressionStatement) program.getStatements().get(0);

        Assertions.assertInstanceOf(IntegerLiteralExpression.class, statement.getExpression());
        var integer = (IntegerLiteralExpression) statement.getExpression();

        var value = integer.getValue();
        Assertions.assertEquals(5, value);
        Assertions.assertEquals("5", integer.tokenLiteral());
    }

    private record PrefixTestRecord(String input, String operator, long integerValue) {
    }

    @Test
    void testParsingPrefixExpressions() {
        var prefixTests = List.of(
                new PrefixTestRecord("!5", "!", 5),
                new PrefixTestRecord("- 15;", "-", 15)
        );

        for (PrefixTestRecord(String input, String operator, long integerValue) : prefixTests) {
            Program program = buildProgram(input);

            Assertions.assertEquals(1, program.getStatements().size());

            Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
            var statement = (ExpressionStatement) program.getStatements().get(0);

            Assertions.assertInstanceOf(PrefixExpression.class, statement.getExpression());
            var prefix = (PrefixExpression) statement.getExpression();
            Assertions.assertEquals(operator, prefix.getOperator());

            var right = prefix.getRight();
            testIntegerLiteral(right, integerValue);
        }
    }

    private record InfixTestRecord(String input, long leftValue, String operator, long rightValue) {
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
                new InfixTestRecord("5 != 5;", 5, "!=", 5)
        );

        for (InfixTestRecord(String input, long leftValue, String operator, long rightValue) : infixTests) {
            var program = buildProgram(input);

            Assertions.assertEquals(1, program.getStatements().size());

            Assertions.assertInstanceOf(ExpressionStatement.class, program.getStatements().get(0));
            var statement = (ExpressionStatement) program.getStatements().get(0);

            Assertions.assertInstanceOf(InfixExpression.class, statement.getExpression());
            var infix = (InfixExpression) statement.getExpression();
            Assertions.assertEquals(operator, infix.getOperator());

            var left = infix.getLeft();
            testIntegerLiteral(left, leftValue);

            var right = infix.getRight();
            testIntegerLiteral(right, rightValue);
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
                        "(3 + 4)\n((-5) * 5)" // TODO Here, we are inserting a \n since there are two Statements. Should we stick to the book?
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
                )
        );

        for (List<String> test : tests) {
            var program = buildProgram(test.get(0));

            Assertions.assertEquals(test.get(1), program.toString());
        }
    }

    private void testIntegerLiteral(Expression expression, Long expectedValue) {
        if (expression instanceof IntegerLiteralExpression integerLiteralExpression) {
            Assertions.assertEquals(expectedValue, integerLiteralExpression.getValue());
            Assertions.assertEquals(expectedValue.toString(), integerLiteralExpression.tokenLiteral());
        } else {
            throw new AssertionError(expression.getClass().getSimpleName() + " is not instance of IntegerLiteralExpression");
        }
    }

    private Program buildProgram(String input) {
        var l = new Lexer(input);
        var p = new Parser(l);
        var program = p.parseProgram();

        checkParseErrors(p);

        return program;
    }

    private void testLetStatement(Statement statement, String name) {
        Assertions.assertEquals(TokenType.LET.token().literal(), statement.tokenLiteral());

        if (statement instanceof LetStatement letStatement) {
            Assertions.assertEquals(name, letStatement.getName().getValue());
            Assertions.assertEquals(name, letStatement.getName().tokenLiteral());
        } else {
            throw new AssertionError(statement.getClass().getSimpleName() + " is not instance of LetStatement");
        }
    }

    private void checkParseErrors(Parser p) {
        if (!p.errors.isEmpty()) {
            StringBuilder errorMessage = new StringBuilder("Parser encountered errors:\n");

            for (var error : p.errors) {
                errorMessage.append(error);
            }

            throw new AssertionError(errorMessage.toString());
        }
    }
}
