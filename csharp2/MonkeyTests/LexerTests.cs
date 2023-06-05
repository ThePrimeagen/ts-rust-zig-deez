using FluentAssertions;
using Monkey;
using Xunit;

namespace MonkeyTests;

public class LexerTests
{
    [Fact]
    public void Get_NextToken_Complete()
    {
        const string testInput = """
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

        var lexer = new Lexer(testInput);

        var tokens = new List<Token>
        {
            new(TokenType.Let),
            new(TokenType.Ident, "five"),
            new(TokenType.Assign),
            new(TokenType.Int, "5"),
            new(TokenType.Semicolon),
            new(TokenType.Let),
            new(TokenType.Ident, "ten"),
            new(TokenType.Assign),
            new(TokenType.Int, "10"),
            new(TokenType.Semicolon),
            new(TokenType.Let),
            new(TokenType.Ident, "add"),
            new(TokenType.Assign),
            new(TokenType.Function),
            new(TokenType.Lparen),
            new(TokenType.Ident, "x"),
            new(TokenType.Comma),
            new(TokenType.Ident, "y"),
            new(TokenType.Rparen),
            new(TokenType.LSquirly),
            new(TokenType.Ident, "x"),
            new(TokenType.Plus),
            new(TokenType.Ident, "y"),
            new(TokenType.Semicolon),
            new(TokenType.RSquirly),
            new(TokenType.Semicolon),
            new(TokenType.Let),
            new(TokenType.Ident, "result"),
            new(TokenType.Assign),
            new(TokenType.Ident, "add"),
            new(TokenType.Lparen),
            new(TokenType.Ident,"five"),
            new(TokenType.Comma),
            new(TokenType.Ident, "ten"),
            new(TokenType.Rparen),
            new(TokenType.Semicolon),
            new(TokenType.Bang),
            new(TokenType.Dash),
            new(TokenType.ForwardSlash),
            new(TokenType.Asterisk),
            new(TokenType.Int,"5"),
            new(TokenType.Semicolon),
            new(TokenType.Int,"5"),
            new(TokenType.LessThan),
            new(TokenType.Int,"10"),
            new(TokenType.GreaterThan),
            new(TokenType.Int,"5"),
            new(TokenType.Semicolon),
            new(TokenType.If),
            new(TokenType.Lparen),
            new(TokenType.Int,"5"),
            new(TokenType.LessThan),
            new(TokenType.Int,"10"),
            new(TokenType.Rparen),
            new(TokenType.LSquirly),
            new(TokenType.Return),
            new(TokenType.True),
            new(TokenType.Semicolon),
            new(TokenType.RSquirly),
            new(TokenType.Else),
            new(TokenType.LSquirly),
            new(TokenType.Return),
            new(TokenType.False),
            new(TokenType.Semicolon),
            new(TokenType.RSquirly),
            new(TokenType.Int,"10"),
            new(TokenType.Equal),
            new(TokenType.Int,"10"),
            new(TokenType.Semicolon),
            new(TokenType.Int,"10"),
            new(TokenType.NotEqual),
            new(TokenType.Int,"9"),
            new(TokenType.Semicolon),

            new(TokenType.Eof),
        };

        foreach (var token in tokens)
        {
            var nextToken = lexer.NextToken();
            token.Should().Be(nextToken);
        }
    }
}
