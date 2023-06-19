using Monkey.Lexing;
using Monkey.Parsing;
using Xunit.Abstractions;

namespace MonkeyTests;

[UsesVerify]
public class ParserTests
{
    private readonly ITestOutputHelper _output;
    public ParserTests(ITestOutputHelper output) => _output = output;

    [Theory]
    [InlineData("let a = 4;", "assign integer")]
    [InlineData("let a = foo;", "assign identifier")]
    [InlineData("return foo;", "return")]
    [InlineData("-4", "prefix operators")]
    [InlineData("!!!!a", "multiple prefix operators")]
    [InlineData("69 * 420", "math operation")]
    [InlineData("5 + 69 * 420 - 10", "math precedence")]
    [InlineData("5 + --10", "prefix precedence")]
    [InlineData("let foo = 4 > 8 != true;", "booleans")]
    [InlineData("let foo = (bar + 69) * 420 - 10 / 4;", "assign complex expression")]
    [InlineData("let foo = if (bar > 8) { 74 } else { return 4 * 6 - 9; };", "assign conditional")]
    public Task ParseStatements_ShouldParseSingleLineStatement(string input, string name)
        => Verify(Parse(input)).UseParameters(name);

    [Theory]
    [InlineData("fn(a, b) { a + b }", "simple function")]
    public Task ParseStatements_ShouldParseFunction(string input, string name)
        => Verify(Parse(input)).UseParameters(name);

    [Theory]
    [InlineData("add(a, b)", "simple function")]
    public Task ParseStatements_ShouldParseFunctionCall(string input, string name)
        => Verify(Parse(input)).UseParameters(name);

    [Fact]
    public Task ParseStatements_ShouldParseSimpleProgram()
    {
        const string input = """
            let x = 69;
            let y = 42;
            return (x + y) * (x - y);
            """;

        return Verify(Parse(input));
    }

    [Fact]
    public Task ParseStatements_ShouldParseAnExampleProgram()
    {
        const string input = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            let foo = if (result > ten) { add(42, 69) } else { 24 * 96 };

            10 == 10;
            10 != 9;
            """;

        return Verify(Parse(input));
    }

    private string Parse(string input)
    {
        var output = Parser.Parse(Lexer.ParseTokens(input)).Format();
        _output.WriteLine(output);
        return output;
    }
}