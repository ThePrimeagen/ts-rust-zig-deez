using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using Monkey;

BenchmarkRunner.Run<LexerBench>();

[MemoryDiagnoser]
public class LexerBench
{
    static readonly string _input = """
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);
    """;

    [Benchmark(Baseline = true)]
    public void Lexer()
    {
        var lexer = new Lexer(_input);
        var token = lexer.NextToken();
        while (token.Type != TokenType.Eof)
            token = lexer.NextToken();
    }
}
