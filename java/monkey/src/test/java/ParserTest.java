import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.Lexer;
import root.Parser;
import root.TokenType;
import root.ast.LetStatement;
import root.ast.Statement;

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
        Assertions.assertEquals(3, program.statements.size());

        var expectedIdentifiers = List.of("x", "y", "foobar");

        var index = 0;
        for (var statement : program.statements) {
            testStatement(statement, expectedIdentifiers.get(index++));
        }
    }

    private void testStatement(Statement statement, String name) {
        Assertions.assertEquals(TokenType.LET.token().literal(), statement.tokenLiteral());

        if (statement instanceof LetStatement letStatement) {
            Assertions.assertEquals(name, letStatement.name.value);
            Assertions.assertEquals(name, letStatement.name.tokenLiteral());
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
