using FluentAssertions;
using Monkey;
using Xunit;

namespace MonkeyTests;

public class AstTests
{
    [Fact]
    public void TestAst()
    {
        // let somevar = anotherVar;
        var statements = new IStatement[]
        {
            new LetStatement("somevar") { Value = new Identifier("anotherVar") },
        };

        var program = new Ast(statements);
        var result = program.ToString();
        result.Should().Be("let somevar = anotherVar;");
    }
}
