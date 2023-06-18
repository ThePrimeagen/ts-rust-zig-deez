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
            new(TokenType.Let, "let"),
            new(TokenType.Ident, "five"),
            new(TokenType.Assign, "="),
            new(TokenType.Int, "5"),
            new(TokenType.Semicolon, ";"),
            new(TokenType.Let, "let"),
            new(TokenType.Ident, "ten"),
            new(TokenType.Assign, "="),
            new(TokenType.Int, "10"),
            new(TokenType.Semicolon, ";"),
            new(TokenType.Let, "let"),
            new(TokenType.Ident, "add"),
            new(TokenType.Assign, "="),
            new(TokenType.Function, "fn"),
            new(TokenType.LParen, "("),
            new(TokenType.Ident, "x"),
            new(TokenType.Comma, ","),
            new(TokenType.Ident, "y"),
            new(TokenType.RParen, ")"),
            new(TokenType.LSquirly, "{"),
            new(TokenType.Ident, "x"),
            new(TokenType.Plus, "+"),
            new(TokenType.Ident, "y"),
            new(TokenType.Semicolon, ";"),
            new(TokenType.RSquirly, "}"),
            new(TokenType.Semicolon, ";"),
            new(TokenType.Let, "let"),
            new(TokenType.Ident, "result"),
            new(TokenType.Assign, "="),
            new(TokenType.Ident, "add"),
            new(TokenType.LParen, "("),
            new(TokenType.Ident,"five"),
            new(TokenType.Comma,","),
            new(TokenType.Ident, "ten"),
            new(TokenType.RParen,")"),
            new(TokenType.Semicolon,";"),
            new(TokenType.Bang,"!"),
            new(TokenType.Minus,"-"),
            new(TokenType.Slash,"/"),
            new(TokenType.Asterisk,"*"),
            new(TokenType.Int,"5"),
            new(TokenType.Semicolon,";"),
            new(TokenType.Int,"5"),
            new(TokenType.LessThan,"<"),
            new(TokenType.Int,"10"),
            new(TokenType.GreaterThan,">"),
            new(TokenType.Int,"5"),
            new(TokenType.Semicolon,";"),
            new(TokenType.If,"if"),
            new(TokenType.LParen,"("),
            new(TokenType.Int,"5"),
            new(TokenType.LessThan,"<"),
            new(TokenType.Int,"10"),
            new(TokenType.RParen,")"),
            new(TokenType.LSquirly,"{"),
            new(TokenType.Return,"return"),
            new(TokenType.True,"true"),
            new(TokenType.Semicolon,";"),
            new(TokenType.RSquirly,"}"),
            new(TokenType.Else,"else"),
            new(TokenType.LSquirly,"{"),
            new(TokenType.Return,"return"),
            new(TokenType.False,"false"),
            new(TokenType.Semicolon,";"),
            new(TokenType.RSquirly,"}"),
            new(TokenType.Int,"10"),
            new(TokenType.Equal, "=="),
            new(TokenType.Int,"10"),
            new(TokenType.Semicolon,";"),
            new(TokenType.Int,"10"),
            new(TokenType.NotEqual,"!="),
            new(TokenType.Int,"9"),
            new(TokenType.Semicolon,";"),

            new(TokenType.Eof,"EOF"),
        };

        foreach (var token in tokens)
        {
            var nextToken = lexer.NextToken();
            token.Should().Be(nextToken);
        }
    }
}
