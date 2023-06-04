using Monkey.Lexing;
using Monkey.Parsing;
using Xunit.Abstractions;

namespace MonkeyTests;

[UsesVerify]
public class LexerTests
{
    private readonly ITestOutputHelper _output;
    public LexerTests(ITestOutputHelper output) => _output = output;

    [Fact]
    public Task ParseTokens_ShouldParseSimpleCharacters()
        => Verify(Parse("=+(){},;"));

    [Fact]
    public Task ParseTokens_ShouldParseDigitsEndingWithLettersAsNumbersAndIdentifiers()
        => Verify(Parse("123456abc"));

    [Fact]
    public Task ParseTokens_ShouldParseDigitsStartingWithLetterAsIdentifier()
        => Verify(Parse("a123456"));

    [Fact]
    public Task ParseTokens_ShouldParseAnExampleProgram()
    {
        const string input = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            """;

        return Verify(Parse(input));
    }

    private string Parse(string input)
    {
        var output = Lexer.ParseTokens(input).Format();
        _output.WriteLine(output);
        return output;
    }
}