using FluentAssertions;
using Monkey;
using Xunit;

namespace MonkeyTests;

public class AstTests
{
    [Fact]
    public void ProgramString_Matches_LiteralString()
    {
        var statements = new IStatement[]
        {
            new LetStatement
            {
                Token = new Token(TokenType.Let, "let"),
                Name =  new Identifier
                {
                    Token = new Token(TokenType.Ident, "somevar"),
                    Value = "myvar"
                },
                Value = new Identifier {
                    Token = new Token(TokenType.Ident, "anotherVar"),
                    Value = "anotherVar"
                }
            },
        };

        var program = new Ast(statements);
        var result = program.ToString();
        result.Should().Be("let somevar = anotherVar;");
    }
}
