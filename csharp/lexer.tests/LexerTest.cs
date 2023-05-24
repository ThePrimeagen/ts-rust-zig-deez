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
        
        Assert.That(tokens, Is.EqualTo(expectedResult));
    }
}