namespace Monkey;

using PrefixFn = Func<IExpression>;
using InfixFn = Func<IExpression, IExpression>;

enum Precedence
{
    Lowest,      // starting
    Equality,    // ==
    Comparison,  // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call         // func(X)
}

class Parser
{
    readonly Lexer _lexer;
    readonly List<string> _errors = new();

    Token _curToken;
    Token _peekToken;

    readonly Dictionary<TokenType, PrefixFn> _prefixFns = new();
    readonly Dictionary<TokenType, InfixFn> _infixFns = new();
    static readonly Dictionary<TokenType, Precedence> Precedences = new()
    {
        [TokenType.Equal] = Precedence.Equality,
        [TokenType.NotEqual] = Precedence.Equality,
        [TokenType.LessThan] = Precedence.Comparison,
        [TokenType.GreaterThan] = Precedence.Comparison,
        [TokenType.Plus] = Precedence.Sum,
        [TokenType.Minus] = Precedence.Sum,
        [TokenType.Asterisk] = Precedence.Product,
        [TokenType.Slash] = Precedence.Product,
    };

    public Parser(Lexer lexer)
    {
        _lexer = lexer;
        NextToken();
        NextToken();

        _prefixFns.Add(TokenType.Ident, ParseIdentifierExpression);
        _prefixFns.Add(TokenType.Int, ParseIntegerExpression);
        _prefixFns.Add(TokenType.Bang, ParsePrefixExpression);
        _prefixFns.Add(TokenType.Minus, ParsePrefixExpression);

        _infixFns.Add(TokenType.Plus, ParseInfixExpression);
        _infixFns.Add(TokenType.Minus, ParseInfixExpression);
        _infixFns.Add(TokenType.Asterisk, ParseInfixExpression);
        _infixFns.Add(TokenType.Slash, ParseInfixExpression);
        _infixFns.Add(TokenType.Equal, ParseInfixExpression);
        _infixFns.Add(TokenType.NotEqual, ParseInfixExpression);
        _infixFns.Add(TokenType.LessThan, ParseInfixExpression);
        _infixFns.Add(TokenType.GreaterThan, ParseInfixExpression);
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

    LetStatement? ParseLetStatement()
    {
        var token = _curToken;
        if (!ExpectPeek(TokenType.Ident))
            return null;

        var ident = new Identifier { Token = _curToken, Value = _curToken.Literal };
        if (!ExpectPeek(TokenType.Assign))
            return null;

        while (_curToken.Type is not TokenType.Semicolon)
            NextToken();

        return new() { Token = token, Name = ident, Value = null! };
    }

    ReturnStatement? ParseReturnStatement()
    {
        while (_curToken.Type is not TokenType.Semicolon)
            NextToken();

        return new() { Token = Token.Return, Expression = null! };
    }

    ExpressionStatement? ParseExpressionStatement()
    {
        var token = _curToken;
        var expression = ParseExpression(Precedence.Lowest);

        if (_peekToken.Type is TokenType.Semicolon)
            NextToken();

        return new() { Token = token, Expression = expression };
    }

    IExpression? ParseExpression(Precedence precedence)
    {
        if (!_prefixFns.TryGetValue(_curToken.Type, out var prefix))
            return null;

        var expression = prefix();
        while (_peekToken.Type is not TokenType.Semicolon && PeekPrecedence() > precedence)
        {
            if (!_infixFns.TryGetValue(_peekToken.Type, out var infix))
                return expression;

            NextToken();
            expression = infix(expression);
        }

        return expression;
    }

    Identifier ParseIdentifierExpression()
      => new() { Token = _curToken, Value = _curToken.Literal };

    IntegerLiteral ParseIntegerExpression()
    {
        if (!long.TryParse(_curToken.Literal, out var value))
            _errors.Add($"Couldn't parse {_curToken.Literal} as an integer");

        return new() { Token = _curToken, Value = value };
    }

    PrefixExpression ParsePrefixExpression()
    {
        var token = _curToken;
        NextToken();
        var right = ParseExpression(Precedence.Prefix);
        return new() { Token = token, Operator = token.Literal, Right = right };
    }

    InfixExpression ParseInfixExpression(IExpression left)
    {
        var token = _curToken;
        var precedence = CurPrecedence();
        NextToken();
        var right = ParseExpression(precedence);
        return new() { Token = token, Operator = token.Literal, Left = left, Right = right };
    }

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

    Precedence CurPrecedence()
        => Precedences.TryGetValue(_curToken.Type, out var p) ? p : Precedence.Lowest;

    Precedence PeekPrecedence()
        => Precedences.TryGetValue(_peekToken.Type, out var p) ? p : Precedence.Lowest;
}
