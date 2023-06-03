using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using Monkey.Lexing;

BenchmarkRunner.Run<LexerBench>();

[MemoryDiagnoser]
public class LexerBench
{
    private const string Input = """
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        """;

    [Benchmark(Baseline = true)]
    public IToken[] LexerBenchmark() => Lexer.ParseTokens(Input);
}