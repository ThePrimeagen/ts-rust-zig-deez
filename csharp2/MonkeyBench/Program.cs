using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using Monkey;

BenchmarkRunner.Run<LexerBench>();

[MemoryDiagnoser]
public class LexerBench {
    const string Input = """
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);
    """;

    [Benchmark(Baseline = true)]
    public void Lexer() {
        var lexer = new Lexer(Input);
        var token = lexer.NextToken();

        while (token.Type != TokenType.Eof)
            token = lexer.NextToken();
    }

    [Benchmark]
    public void Parser() {
        var lexer = new Lexer(Input);
        var parser = new Parser(lexer);
        _ = parser.ParseProgram();
    }
}
