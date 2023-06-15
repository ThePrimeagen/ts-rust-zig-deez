import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.ast.statements.ExpressionStatement;
import root.lexer.Lexer;
import root.parser.Parser;
import root.TokenType;
import root.ast.*;
import root.ast.expressions.IdentifierExpression;
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

        var l = new Lexer(input);
        var p = new Parser(l);

        var program = p.parseProgram();

        Assertions.assertNotNull(program);
        checkParseErrors(p);
        Assertions.assertEquals(3, program.getStatements().size());

        var expectedIdentifiers = List.of("x", "y", "foobar");

        var index = 0;
        for (var statement : program.getStatements()) {
            testStatement(statement, expectedIdentifiers.get(index++));
        }
    }

    @Test
    void testReturnStatements() {
        var input = """
                return 5;
                return 10;
                return 993322;""";

        var l = new Lexer(input);
        var p = new Parser(l);

        var program = p.parseProgram();

        Assertions.assertNotNull(program);
        checkParseErrors(p);
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

        var l = new Lexer(input);
        var p = new Parser(l);

        var program = p.parseProgram();

        checkParseErrors(p);

        Assertions.assertEquals(1, program.getStatements().size());
        var statement = program.getStatements().get(0);
        Assertions.assertInstanceOf(ExpressionStatement.class, statement);
        var ident = ((ExpressionStatement) statement).getExpression();
        Assertions.assertInstanceOf(IdentifierExpression.class, ident);
        var value = ((IdentifierExpression) ident).getValue();
        Assertions.assertEquals("foobar", value);
        Assertions.assertEquals("foobar", ident.tokenLiteral());
    }

    private void testStatement(Statement statement, String name) {
        Assertions.assertEquals(TokenType.LET.token().literal(), statement.tokenLiteral());

        if (statement instanceof LetStatement letStatement) {
            Assertions.assertEquals(name, letStatement.getName().getValue());
            Assertions.assertEquals(name, letStatement.getName().tokenLiteral());
        } else {
            throw new AssertionError("statement not instance of LetStatement");
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
