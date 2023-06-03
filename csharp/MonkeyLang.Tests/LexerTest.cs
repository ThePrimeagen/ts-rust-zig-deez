namespace MonkeyLang.Tests;

public class Tests
{
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public void Test1()
    {
        const string testInput = "=+(){},;";

        var lexer = new Lexer(testInput);

        var expectedResult = new List<Token>
        {
            new Token(TokenType.Assign),
            new Token(TokenType.Plus),
            new Token(TokenType.LParen),
            new Token(TokenType.RParen),
            new Token(TokenType.LSquirly),
            new Token(TokenType.RSquirly),
            new Token(TokenType.Comma),
            new Token(TokenType.Semicolon),
            new Token(TokenType.Eof)
        };

        foreach (var expected in expectedResult)
        {
            var result = lexer.NextToken();
            Assert.That(result, Is.EqualTo(expected));
        }
    }

    [Test]
    public void Test2()
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

        var expectedResult = new List<Token>
        {
            new Token(TokenType.Let),        // let
            new Token(TokenType.Identifier, "five"), // five
            new Token(TokenType.Assign),      // =
            new Token(TokenType.Integer, "5"),    // 5
            new Token(TokenType.Semicolon),  // ;
            new Token(TokenType.Let),        // let
            new Token(TokenType.Identifier, "ten"), // ten
            new Token(TokenType.Assign),      // =
            new Token(TokenType.Integer, "10"),    // 10
            new Token(TokenType.Semicolon),  // ;
            new Token(TokenType.Let),        // let
            new Token(TokenType.Identifier, "add"), // add
            new Token(TokenType.Assign),      // =
            new Token(TokenType.Function),   // fn
            new Token(TokenType.LParen),     // (
            new Token(TokenType.Identifier, "x"), // x
            new Token(TokenType.Comma),      // ,
            new Token(TokenType.Identifier, "y"), // y
            new Token(TokenType.RParen),     // )
            new Token(TokenType.LSquirly),   // {
            new Token(TokenType.Identifier, "x"), // x
            new Token(TokenType.Plus),       // +
            new Token(TokenType.Identifier, "y"), // y
            new Token(TokenType.Semicolon),  // ;
            new Token(TokenType.RSquirly),   // }
            new Token(TokenType.Semicolon),  // ;
            new Token(TokenType.Let),        // let
            new Token(TokenType.Identifier, "result"), // result
            new Token(TokenType.Assign),      // =
            new Token(TokenType.Identifier, "add"), // add
            new Token(TokenType.LParen),     // (
            new Token(TokenType.Identifier, "five"), // five
            new Token(TokenType.Comma),      // ,
            new Token(TokenType.Identifier, "ten"), // ten
            new Token(TokenType.RParen),     // )
            new Token(TokenType.Semicolon),  // ;
            new Token(TokenType.Bang),  // !
            new Token(TokenType.Minus),  // -
            new Token(TokenType.Slash),  // /
            new Token(TokenType.Asterisk),  // *
            new Token(TokenType.Integer, "5"), // 5
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.Integer, "5"), // 5
            new Token(TokenType.LT), // <
            new Token(TokenType.Integer, "10"), // 10
            new Token(TokenType.GT), // >
            new Token(TokenType.Integer, "5"), // 5
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.If), // if
            new Token(TokenType.LParen), // (
            new Token(TokenType.Integer, "5"), // 5
            new Token(TokenType.LT), // <
            new Token(TokenType.Integer, "10"), // 10
            new Token(TokenType.RParen), // )
            new Token(TokenType.LSquirly), // {
            new Token(TokenType.Return), // return
            new Token(TokenType.True), // true
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.RSquirly), // }
            new Token(TokenType.Else), // else
            new Token(TokenType.LSquirly), // {
            new Token(TokenType.Return), // return
            new Token(TokenType.False), // false
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.RSquirly), // }
            new Token(TokenType.Integer, "10"), // 10
            new Token(TokenType.EQ), // ==
            new Token(TokenType.Integer, "10"), // 10
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.Integer, "10"), // 10
            new Token(TokenType.NOT_EQ), // !=
            new Token(TokenType.Integer, "9"), // 9
            new Token(TokenType.Semicolon), // ;
            new Token(TokenType.Eof),
        };

        foreach (var expected in expectedResult)
        {
            var result = lexer.NextToken();
            Assert.That(result, Is.EqualTo(expected));
        }
    }
}