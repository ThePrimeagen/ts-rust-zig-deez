namespace Monkey;

class Parser
{
    readonly Lexer _lexer;
    readonly List<string> _errors = new();

    Token _curToken;
    Token _peekToken;

    public Parser(Lexer lexer)
    {
        _lexer = lexer;
        NextToken();
        NextToken();
    }

    public Ast ParseProgram()
    {
        var statements = new List<IStatement>();
        while (_curToken.Type is not TokenType.Eof)
        {
            IStatement? statement = _curToken switch
            {
                { Type: TokenType.Let } => ParseLetStatement(),
                { Type: TokenType.Return } => ParseReturnStatement(),
                _ => ParseExpressionStatement(),
            };

            if (statement is not null)
                statements.Add(statement);

            NextToken();
        }

        return new Ast(statements);
    }

    public IEnumerable<string> Errors
        => _errors.ToList();

    void NextToken()
    {
        _curToken = _peekToken;
        _peekToken = _lexer.NextToken();
    }

    bool ExpectPeek(TokenType type)
    {
        if (_peekToken.Type == type)
        {
            NextToken();
            return true;
        }

        _errors.Add($"Expected next token to be '{type}' and got '{_peekToken.Type}'");
        return false;
    }

    LetStatement? ParseLetStatement()
    {
        if (!ExpectPeek(TokenType.Ident))
            return null;

        var stmt = new LetStatement(_curToken.Literal);
        if (!ExpectPeek(TokenType.Assign))
            return null;

        // TODO: Add expression

        return _curToken.Type is TokenType.Semicolon ? null : stmt;
    }

    ReturnStatement? ParseReturnStatement()
    {
        var stmt = new ReturnStatement();
        // TODO: Add expression

        return _curToken.Type is TokenType.Semicolon ? null : stmt;
    }

    ExpressionStatement? ParseExpressionStatement()
    {
        return null;
    }
}