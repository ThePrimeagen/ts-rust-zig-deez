namespace lexer.tests;

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
        var tokens = Lexer.ParseTokens(testInput);

        var expectedResult = new List<Tokens>
        {
            Tokens.Equal,
            Tokens.Plus,
            Tokens.LParen,
            Tokens.RParen,
            Tokens.LSquirly,
            Tokens.RSquirly,
            Tokens.Comma,
            Tokens.Semicolon,
            Tokens.Eof
        };
        
        Assert.That(tokens.Select(x => x.Item1), Is.EqualTo(expectedResult));
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
                                """;

        var tokens = Lexer.ParseTokens(testInput);

        var expectedResult = new List<Tokens>
        {
            Tokens.Let,        // let
            Tokens.Identifier, // five
            Tokens.Equal,      // =
            Tokens.Integer,    // 5
            Tokens.Semicolon,  // ;

            Tokens.Let,        // let
            Tokens.Identifier, // ten
            Tokens.Equal,      // =
            Tokens.Integer,    // 10
            Tokens.Semicolon,  // ;

            Tokens.Let,        // let
            Tokens.Identifier, // add
            Tokens.Equal,      // =
            Tokens.Function,   // fn
            Tokens.LParen,     // (
            Tokens.Identifier, // x
            Tokens.Comma,      // ,
            Tokens.Identifier, // y
            Tokens.RParen,     // )
            Tokens.LSquirly,   // {
            Tokens.Identifier, // x
            Tokens.Plus,       // +
            Tokens.Identifier, // y
            Tokens.Semicolon,  // ;
            Tokens.RSquirly,   // }
            Tokens.Semicolon,  // ;

            Tokens.Let,        // let
            Tokens.Identifier, // result
            Tokens.Equal,      // =
            Tokens.Identifier, // add
            Tokens.LParen,     // (
            Tokens.Identifier, // five
            Tokens.Comma,      // ,
            Tokens.Identifier, // ten
            Tokens.RParen,     // )
            Tokens.Semicolon,  // ;
            Tokens.Eof
        };

        
        Assert.That(tokens.Select(x => x.Item1), Is.EqualTo(expectedResult));
    }
}