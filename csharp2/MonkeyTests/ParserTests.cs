using FluentAssertions;
using Monkey;
using Xunit;

namespace MonkeyTests;

public class ParserTests {

    [Theory]
    [InlineData("let x = 5;", "x")]
    [InlineData("let y = 5;", "y")]
    [InlineData("let foobar = 838383;", "foobar")]
    public void Parsing_LetStatements(string input, string expected) {
        // Arrange & Act
        var (ast, errors) = ParseProgram(input);

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<LetStatement>().Subject;

        stmt.Token.Type.Should().Be(TokenType.Let);
        stmt.Name.Value.Should().Be(expected);
        stmt.Name.ToString().Should().Be(expected);
    }

    [Theory]
    [InlineData("return 5;")]
    [InlineData("return 10;")]
    [InlineData("return 993322;")]
    public void Parsing_LetReturnStatements(string input) {
        // Arrange & Act
        var (ast, errors) = ParseProgram(input);

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<ReturnStatement>().Subject;

        stmt.Token.Type.Should().Be(TokenType.Return);
    }

    [Fact]
    public void Parsing_IdentifierExpression() {
        // Arrange & Act
        var (ast, errors) = ParseProgram("foobar;");

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<ExpressionStatement>()
                .Subject.Expression.Should()
                .NotBeNull().And
                .BeAssignableTo<Identifier>().Subject;

        stmt.Value.Should().Be("foobar");
        stmt.Token.Literal.Should().Be("foobar");
    }

    [Fact]
    public void Parsing_IntegerLiteralExpression() {
        // Arrange & Act
        var (ast, errors) = ParseProgram("5;");

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<ExpressionStatement>()
                .Subject.Expression.Should()
                .NotBeNull().And
                .BeAssignableTo<IntegerLiteral>().Subject;

        stmt.Value.Should().Be(5);
        stmt.Token.Literal.Should().Be("5");
    }

    [Theory]
    [InlineData("!5", "!", 5)]
    [InlineData("-15", "-", 15)]
    public void Parsing_PrefixExpression(string input, string op, long value) {
        // Arrange & Act
        var (ast, errors) = ParseProgram(input);

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<ExpressionStatement>()
                .Subject.Expression.Should()
                .NotBeNull().And
                .BeAssignableTo<PrefixExpression>().Subject;

        stmt.Operator.Should().Be(op);
        IntegerLiteralTest(stmt.Right, value);
    }

    static void IntegerLiteralTest(IExpression expression, long value) {
        var stmt = expression.Should()
            .BeAssignableTo<IntegerLiteral>().Subject;

        stmt.Value.Should().Be(value);
        stmt.ToString().Should().Be(value.ToString());
    }

    [Theory]
    [InlineData("5 + 5;", 5, "+", 5)]
    [InlineData("5 - 5;", 5, "-", 5)]
    [InlineData("5 * 5;", 5, "*", 5)]
    [InlineData("5 / 5;", 5, "/", 5)]
    [InlineData("5 > 5;", 5, ">", 5)]
    [InlineData("5 < 5;", 5, "<", 5)]
    [InlineData("5 == 5;", 5, "==", 5)]
    [InlineData("5 != 5;", 5, "!=", 5)]
    public void Parsing_InfixExpression(string input, long left, string op, long right) {
        // Arrange & Act
        var (ast, errors) = ParseProgram(input);

        // Assert
        errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(1);

        var stmt = ast.Statements[0].Should()
            .NotBeNull().And
            .BeAssignableTo<ExpressionStatement>()
                .Subject.Expression.Should()
                .NotBeNull().And
                .BeAssignableTo<InfixExpression>().Subject;

        IntegerLiteralTest(stmt.Left, left);
        stmt.Operator.Should().Be(op);
        IntegerLiteralTest(stmt.Right, right);
    }


    [Theory]
    [InlineData("-a * b", "((-a) * b)")]
    [InlineData("!-a", "(!(-a))")]
    [InlineData("a + b + c", "((a + b) + c)")]
    [InlineData("a + b - c", "((a + b) - c)")]
    [InlineData("a * b * c", "((a * b) * c)")]
    [InlineData("a * b / c", "((a * b) / c)")]
    [InlineData("a + b / c", "(a + (b / c))")]
    [InlineData("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")]
    [InlineData("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")]
    [InlineData("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")]
    [InlineData("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")]
    [InlineData("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")]
    public void Parsing_OperatorPrecedence(string input, string expected) {
        // Arrange & Act
        var (ast, errors) = ParseProgram(input);

        // Assert
        errors.Should().HaveCount(0);
        ast.ToString().Should().Be(expected);
    }

    static (Ast ast, IEnumerable<string> errors) ParseProgram(string input) {
        var lexer = new Lexer(input);
        var parser = new Parser(lexer);
        return (parser.ParseProgram(), parser.Errors);
    }
}