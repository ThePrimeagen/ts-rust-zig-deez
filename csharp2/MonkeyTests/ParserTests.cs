using FluentAssertions;
using Monkey;
using Xunit;

namespace MonkeyTests;

public class ParserTests
{
    [Fact]
    public void Parsing_LetStatements_Succeeds()
    {
        // Arrage
        var input = """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """;

        var expected = new[] { "x", "y", "foobar" };
        var lexer = new Lexer(input);
        var parser = new Parser(lexer);

        // Act
        var ast = parser.ParseProgram();

        // Assert
        parser.Errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(3);

        for (var i = 0; i < expected.Length; i++)
        {
            var letStmt = ast.Statements.ElementAtOrDefault(i)
                .Should()
                .BeAssignableTo<LetStatement>().Subject;

            letStmt.Should().NotBeNull();
            letStmt!.Token.Should().Be(Token.Let);
            expected[i].Should().Be(letStmt.Identifier.Token.Literal);
            expected[i].Should().Be(letStmt.Identifier.ToString());
        }
    }

    [Fact]
    public void Parsing_LetReturnStatements_Succeeds()
    {
        // Arrage
        var input = """
        return 5;
        return 10;
        return 993322;
        """;

        var lexer = new Lexer(input);
        var parser = new Parser(lexer);

        // Act
        var ast = parser.ParseProgram();

        // Assert
        parser.Errors.Should().HaveCount(0);
        ast.Statements.Should().HaveCount(3);

        foreach (var stmt in ast.Statements)
        {
            var returnStmt = stmt.Should()
                .NotBeNull().And
                .BeAssignableTo<ReturnStatement>().Subject;

            returnStmt.Token.Should().Be(Token.Return);
        }
    }
}