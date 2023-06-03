using BenchmarkDotNet.Attributes;

namespace MonkeyLang.PerformanceTests;

[MinColumn, MaxColumn, MemoryDiagnoser]
public class LexerTests
{
    [Benchmark]
    public Token SmallCodeTest()
    {
        Token token;
        var lexer = new Lexer(LexerTestCases.SmallCode);

        do
        {
            token = lexer.NextToken();
        }
        while (token.Type is not TokenType.Eof);

        return token;
    }

    [Benchmark]
    public Token LargeCodeTest()
    {
        Token token;
        var lexer = new Lexer(LexerTestCases.LargeCode);

        do
        {
            token = lexer.NextToken();
        }
        while (token.Type is not TokenType.Eof);

        return token;
    }
}
