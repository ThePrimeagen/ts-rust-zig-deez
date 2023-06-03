using BenchmarkDotNet.Attributes;

namespace MonkeyLang.PerformanceTests;

[MinColumn, MaxColumn, MemoryDiagnoser]
public class LexerTests
{
    [Benchmark]
    public TokenInfo SmallCodeTest()
    {
        TokenInfo token;
        var lexer = new Lexer(LexerTestCases.SmallCode);

        do
        {
            token = lexer.NextToken();
        }
        while (token.Type is not Token.Eof);

        return token;
    }

    [Benchmark]
    public TokenInfo LargeCodeTest()
    {
        TokenInfo token;
        var lexer = new Lexer(LexerTestCases.LargeCode);

        do
        {
            token = lexer.NextToken();
        }
        while (token.Type is not Token.Eof);

        return token;
    }
}
