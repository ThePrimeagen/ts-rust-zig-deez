/**
 * Parser and AST structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import lexer;
import std.algorithm.iteration : joiner;
import std.array : appender, Appender;
import std.conv : to;
import std.format : format;
import std.range : empty, enumerate;

import std.stdio : writefln;

/// Function typedef for prefix Pratt parsing function
alias PrefixParseFn = ExpressionNode function(ref Parser parser);

/// Function typedef for infix Pratt parsing function
alias InfixParseFn = ExpressionNode function(ref Parser parser,
        ref ExpressionNode lhs, Precedence prec);

/// Expression precedence
enum Precedence : ubyte
{
    Lowest,
    Ternary, // X ? a : b
    Equals, // ==
    LessGreater, // > | <
    Term, // + | -
    Factor, // * | /
    Unary, // -X | !X
    Call // myFunction(X)
}

/// Group together infix rules with precedence
struct InfixRule
{
    InfixParseFn infix; /// Parsing function for infix operators
    Precedence prec; /// Operator precedence

    /**
     * Constructs the rule for infix expression parsing.
     * Params:
     * infix = the infix operator parsing function
     * prec = the operator precedence
     */
    this(InfixParseFn infix, Precedence prec)
    {
        this.infix = infix;
        this.prec = prec;
    }
}

/// Map reserved for prefix expression rules per token type
immutable PrefixParseFn[TokenTag] prefixRules;

/// Map reserved for infix expression rules per token type
immutable InfixRule[TokenTag] infixRules;

shared static this()
{
    import std.exception : assumeUnique;

    PrefixParseFn[TokenTag] tempPrefixRules = [
        TokenTag.Ident: &parseIdent, TokenTag.Minus: &parseUnary,
        TokenTag.Bang: &parseUnary, TokenTag.Int: &parseInt,
        TokenTag.True: &parseBoolean, TokenTag.False: &parseBoolean,
        TokenTag.LParen: &parseGroupedExpression
    ];

    InfixRule[TokenTag] tempInfixRules = [
        TokenTag.Plus: InfixRule(&parseBinary, Precedence.Term),
        TokenTag.Minus: InfixRule(&parseBinary, Precedence.Term),
        TokenTag.Asterisk: InfixRule(&parseBinary, Precedence.Factor),
        TokenTag.Slash: InfixRule(&parseBinary, Precedence.Factor),
        TokenTag.Eq: InfixRule(&parseBinary, Precedence.Equals),
        TokenTag.NotEq: InfixRule(&parseBinary, Precedence.Equals),
        TokenTag.Lt: InfixRule(&parseBinary, Precedence.LessGreater),
        TokenTag.Gt: InfixRule(&parseBinary, Precedence.LessGreater),
        TokenTag.Semicolon: InfixRule(null, Precedence.Lowest),
        TokenTag.Eof: InfixRule(null, Precedence.Lowest),
        TokenTag.RParen: InfixRule(null, Precedence.Lowest)
    ];

    prefixRules = assumeUnique(tempPrefixRules);
    infixRules = assumeUnique(tempInfixRules);
}

/**
 * Construct identity node in expression.
 * Params: parser = the parser iterating through tokens
 * Returns: the identity node
 */
ExpressionNode parseIdent(ref Parser parser)
{
    const auto start = parser.position;
    parser.skipToken();
    return new IdentifierNode(start);
}

/**
 * Construct boolean node in expression.
 * Params: parser = the parser iterating through tokens
 * Returns: the boolean node (true | false)
 */
ExpressionNode parseBoolean(ref Parser parser)
{
    const auto start = parser.position;
    parser.skipToken();
    return new BooleanNode(start);
}

/**
 * Parse unary expression (prefix for now).
 * Params: parser = the parser iterating through tokens
 * Returns: the unary expression node
 */
ExpressionNode parseUnary(ref Parser parser)
{
    const auto start = parser.position;
    parser.skipToken();

    auto expr = parser.parseExpression(Precedence.Unary);
    return new PrefixExpressionNode(start, expr);
}

/**
 * Construct integer node in expression.
 * Params: parser = the parser iterating through tokens
 * Returns: the integer node
 */
ExpressionNode parseInt(ref Parser parser)
{
    const auto start = parser.position;
    parser.skipToken();
    return new IntNode(start);
}

/**
 * Parse binary expression.
 * Params:
 * parser = the parser iterating through tokens
 * lhs = the left hand side expression
 * prec = the current operator precedence
 * Returns: the binary expression node
 */
ExpressionNode parseBinary(ref Parser parser, ref ExpressionNode lhs, Precedence prec)
{
    const auto start = parser.position;
    parser.skipToken();

    auto rhs = parser.parseExpression(cast(int)(prec) + 1);
    return new InfixExpressionNode(start, lhs, rhs);
}

/// Parse expressions grouped by parentheses
ExpressionNode parseGroupedExpression(ref Parser parser)
{
    parser.skipToken();
    auto expr = parser.parseExpression(Precedence.Lowest);

    if (parser.tokenTags[][parser.position] != TokenTag.RParen)
    {
        writefln("Expected ')' at position %d; got %s instead",
                parser.position, parser.tokenTags[][parser.position]);
        return null;
    }

    parser.skipToken();
    return expr;
}

/// Most fundamental node
/// TODO: use emplace for custom class allocation
class ParseNode
{
    const ulong mainIdx; /// First index of expression

    /**
     * Constructs expression statement.
     * Params: mainIdx = the starting index of the expression
     */
    this(ulong mainIdx)
    {
        this.mainIdx = mainIdx;
    }

    /**
     * Interface to show the main token in the node.
     * Params: lexer = the lexer context for token representation
     * Returns: the string representation of the main node token
     */
    string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Statement parse node
class StatementNode : ParseNode
{
    /// Constructs generic statement
    this(ulong mainIdx)
    {
        super(mainIdx);
    }

    /**
     * Interface to show the AST node.
     * Params: lexer = the lexer context for token representation
     * Returns: the string representation of the node
     */
    abstract string show(ref Lexer lexer);
}

/// Expression parse node
class ExpressionNode : ParseNode
{
    /// Constructs generic expression
    this(ulong mainIdx)
    {
        super(mainIdx);
    }

    /**
     * Interface to show the AST node.
     * Params: lexer = the lexer context for token representation
     * Returns: the string representation of the node
     */
    abstract string show(ref Lexer lexer);
}

/// Wrapper for ExpressionNodes
class ExpressionStatement : StatementNode
{
    ExpressionNode value; /// Expression node reference

    /**
     * Constructs expression statement.
     * Params:
     * mainIdx = the starting index of the expression
     * value = The expression creating the value
     */
    this(ulong mainIdx, ref ExpressionNode value)
    {
        super(mainIdx);
        this.value = value;
    }

    override string show(ref Lexer lexer)
    {
        return (this.value !is null) ? value.show(lexer) ~ ";" : "";
    }
}

/// Node for let statements
class LetStatement : StatementNode
{
    ExpressionNode value; /// Expression node reference

    /**
     * Constructs let statement.
     * Params:
     * mainIdx = the index of the variable initialized
     * value = The expression initializing the variable
     */
    this(ulong mainIdx, ref ExpressionNode value)
    {
        super(mainIdx);
        this.value = value;
    }

    /// Show the let statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Let);
    }

    /// Create statement string
    override string show(ref Lexer lexer)
    {
        return format("let %s = %s;", super.tokenLiteral(lexer),
                (this.value !is null) ? this.value.show(lexer) : "");
    }
}

/// Node for return statements
class ReturnStatement : StatementNode
{
    ExpressionNode value; /// Expression node reference

    /**
     * Constructs return statement.
     * Params:
     * mainIdx = the index of the statement start
     * value = The expression describing the value
     */
    this(ulong mainIdx, ref ExpressionNode value)
    {
        super(mainIdx);
        this.value = value;
    }

    /// Show the return statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Return);
    }

    /// Create statement string
    override string show(ref Lexer lexer)
    {
        return format("return%s;", (this.value !is null) ? ' ' ~ this.value.show(lexer) : "");
    }
}

/// Identifier node for expressions
class IdentifierNode : ExpressionNode
{
    /**
     * Constructs identifier nodes.
     * Params: mainIdx = the identifier token index
     */
    this(ulong mainIdx)
    {
        super(mainIdx);
    }

    /// Show the identifier tag
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Ident);
    }

    /// Create identifier string
    override string show(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Integer node for expressions
class IntNode : ExpressionNode
{
    /**
     * Constructs int nodes.
     * Params: mainIdx = the int token index
     */
    this(ulong mainIdx)
    {
        super(mainIdx);
    }

    /// Show the main integer
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Int);
    }

    /// Create int string
    override string show(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Boolean node for expressions
class BooleanNode : ExpressionNode
{
    /**
     * Constructs boolean nodes.
     * Params: mainIdx = the boolean token index
     */
    this(ulong mainIdx)
    {
        super(mainIdx);
    }

    /// Show the main boolean value
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(lexer.tokens.tag[][mainIdx]);
    }

    /// Create boolean string
    override string show(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Unary prefix operator node for expressions
class PrefixExpressionNode : ExpressionNode
{
    ExpressionNode expr; /// Primary expression

    /**
     * Constructs the unary prefix expression
     * Params:
     * mainIdx = the index of the operator tag
     * expr = the primary expression
     */
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx);
        this.expr = expr;
    }

    /// Create expression string
    override string show(ref Lexer lexer)
    {
        return format("%s%s", lexer.tagRepr(mainIdx), (this.expr !is null) ? expr.show(lexer) : "");
    }

}

/// Infix binary operator node for expressions
class InfixExpressionNode : ExpressionNode
{
    ExpressionNode lhs; /// lhs expression
    ExpressionNode rhs; /// rhs expression

    /**
     * Constructs the binary operator expression
     * Params:
     * mainIdx = the index of the operator tag
     * lhs = the left hand side expression
     * rhs = the right hand side expression
     */
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    /// Create expression string
    override string show(ref Lexer lexer)
    {
        return format("(%s%s%s)", (this.lhs !is null) ? lhs.show(lexer) ~ " " : "",
                lexer.tagRepr(mainIdx), (this.rhs !is null) ? " " ~ rhs.show(lexer) : "");
    }
}

/// Listing for node collection
struct Program
{
    Appender!(StatementNode[]) statements; /// List of nodes

    /**
     * Constructs the program storing statements.
     * Params: tokenCount = The number of tokens covered by the program
     */
    this(ulong tokenCount)
    {
        this.statements = appender!(StatementNode[])();

        // On average, (n / 2) + 1 statements are needed for all tokens
        const auto initialSize = (tokenCount / 2) + 1;

        this.statements.reserve(initialSize);
    }

    /**
     * Show the entire program and its statements.
     * Params: lexer = the lexer context for token representation
     * Returns: the representation of the program statement
     */
    string tokenLiteral(ref Lexer lexer)
    {
        auto nodeList = this.statements[];

        if (nodeList.length > 0)
        {
            return nodeList[0].tokenLiteral(lexer);
        }
        else
        {
            return "";
        }
    }

    /// Entire program representation for debugging purposes
    string show(ref Lexer lexer)
    {
        auto reprBuilder = appender!(string[]);
        reprBuilder.reserve(16);

        foreach (i, statement; this.statements[].enumerate(0))
        {
            auto repr = statement.show(lexer);
            if (repr !is null && repr != "")
            {
                reprBuilder.put(repr);
            }
        }

        return reprBuilder[].joiner("\n").to!string;
    }
}

/// Encapsulates token parsing
struct Parser
{
private:
    ulong position = 0; /// Current token cursor
    ulong peekPosition = 1; /// Lookahead cursor (after current token)
    const Lexer lexer; /// Lexer instance with token list
    const TokenTag[] tokenTags; /// Alias for tokens in lexer
    const ulong tokenCount; /// Cached token length

public:
    Appender!(string[]) errors; /// List of parse errors
    Program program; /// Container for program statements

    /**
     * Constructs the parser.
     * Params: lexer = the lexer with tokens to parse
     */
    this(const ref Lexer lexer)
    {
        this.lexer = lexer;
        this.tokenTags = this.lexer.tokens.tag[];
        this.tokenCount = this.tokenTags.length;
        this.program = Program(tokenCount);
    }

    /**
     * Advances in a token from the token list.
     */
    void skipToken()
    {
        this.position = this.peekPosition;
        this.peekPosition++;
    }

    /**
     * Skip a certain number of tokens in the parser.
     * Params: count = the number of tokens to skip
     */
    void skipTokens(ulong count)
    {
        if (count < 1)
        {
            return;
        }
        this.peekPosition += count - 1;
        this.position = this.peekPosition;
        this.peekPosition++;
    }

    /**
     * Seeks a token ahead of the current position.
     * Returns: the token ahead of the current position
     */
    TokenTag peek()
    {
        if (this.peekPosition < this.tokenCount)
        {
            return this.tokenTags[this.peekPosition];
        }
        else
        {
            return TokenTag.Eof;
        }
    }

    /**
     * Pratt parser to scan for expression nodes.
     * Params: prec = the starting operator precedence
     * Returns: the node associated with the expression
     */
    ExpressionNode parseExpression(int prec = Precedence.Ternary)
    {
        // Process prefix expressions
        TokenTag token = tokenTags[this.position];
        if (token !in prefixRules)
        {
            errors.put("Expected expression");
            this.skipToken();
            return null;
        }

        auto expr = prefixRules[token](this);

        // Process infix expressions with LHS as reference expression
        while (this.position < this.tokenCount)
        {
            token = tokenTags[this.position];
            if (token !in infixRules)
            {
                errors.put(format("Invalid token %s for infix expression", token));
                break;
            }

            const auto infixRule = infixRules[token];
            const auto currentPrec = infixRule.prec;

            if (currentPrec < prec || infixRule.infix is null)
            {
                break;
            }

            expr = infixRule.infix(this, expr, currentPrec);
        }

        return expr;
    }

    /// Parse expressions as statements
    ExpressionStatement parseExpressionStatement()
    {
        const auto start = this.position;
        auto expr = this.parseExpression();

        if (tokenTags[this.position] == TokenTag.Semicolon)
        {
            this.skipToken();
        }

        return new ExpressionStatement(start, expr);
    }

    /// Parse let statements
    LetStatement parseLetStatement()
    {
        this.skipToken();

        // Validate beginning of statement
        static const auto expectedTags = [TokenTag.Ident, TokenTag.Assign];

        const auto start = this.position;
        const auto sliceEnd = start + expectedTags.length;

        if (sliceEnd > tokenTags.length)
        {
            errors.put("Not enough tokens for Let statement");
            return null;
        }

        const auto tokenSlice = tokenTags[start .. sliceEnd];

        foreach (i, tag; expectedTags.enumerate(0))
        {
            if (tag != tokenSlice[i])
            {
                errors.put(format("Expected next token to be %s in %s statement, got %s instead",
                        tag, TokenTag.Let, tokenSlice[i]));

                this.skipTokens(i);

                while (this.position < this.tokenCount
                        && tokenTags[this.position] != TokenTag.Semicolon)
                {
                    this.skipToken();
                }

                return null;
            }
        }

        this.skipTokens(2);

        // Parse expression as part of let statement
        auto value = this.parseExpression();

        if (tokenTags[this.position] == TokenTag.Semicolon)
        {
            this.skipToken();
        }

        return new LetStatement(start, value);
    }

    /// Parse return statements
    ReturnStatement parseReturnStatement()
    {
        const auto start = this.position;
        this.skipToken();

        // Parse expression as part of let statement
        auto value = this.parseExpression();

        if (tokenTags[this.position] == TokenTag.Semicolon)
        {
            this.skipToken();
        }

        return new ReturnStatement(start, value);
    }

    /// Parse if statements
    StatementNode parseIfStatement()
    {
        return null;
    }

    /// Parse token list to create statement AST nodes
    void parseProgram()
    {
        while (this.position < this.tokenCount)
        {
            const auto token = tokenTags[this.position];
            StatementNode statement = null;

            switch (token) with (TokenTag)
            {
            case Let:
                statement = this.parseLetStatement();
                break;
            case Return:
                statement = this.parseReturnStatement();
                break;
            case If:
                statement = this.parseIfStatement();
                break;
            case Semicolon:
                // Go straight to next token on semicolon
                this.skipToken();
                continue;
            case Eof:
                // Abort parsing on EOF
                return;
            default:
                statement = this.parseExpressionStatement();
            }

            if (statement !is null)
            {
                this.program.statements.put(statement);
            }
        }
    }
}

/** Parser tests */

/// Helper function that validates parsing a valid program.
private void validateParseProgram(const string expected, ref Lexer lexer, ref Parser parser)
{
    assert(parser.errors[].length == 0,
            format("Error list %s must be empty for program", parser.errors[]));

    const auto programListing = parser.program.show(lexer);
    assert(expected == programListing,
            format("Listing --\n%sDoes not match expected output--\n%s", programListing, expected));
}

/// Minimal parser test
unittest
{
    const auto input = "  ";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    assert(parser.program.statements[].empty, "Statement list must be empty for empty program");

    assert(parser.errors[].length == 0,
            format("Error list %s must be empty for empty string", parser.errors[]));
}

/// Single let statement test
unittest
{
    const auto input = "let x = 5;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(input, lexer, parser);
}

/// Single identifier expression statement test
unittest
{
    const auto input = "foobar;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(input, lexer, parser);
}

/// Multiple let statement test
unittest
{
    const auto input = "let x = 5;
let y = true;
let foobar = y;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(input, lexer, parser);
}

/// Prefix expression test
unittest
{
    const auto input = "let x = !5;
let y = -15;
!true;
!false;";

    const auto expected = "let x = !5;
let y = -15;
!true;
!false;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Multiple let statement test
unittest
{
    const auto input = "let x 5;
let = 10;
let 838383";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    assert(parser.program.statements[].empty, "Statement list must be empty for erroneous program");
    assert(parser.errors[].length >= 3, "Error list must not be empty for erroneous program");
}

/// Multiple return statement test
unittest
{
    const auto input = "return 5;
return 10;
return 993322;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(input, lexer, parser);
}

/// Main operator precedence test
unittest
{
    const auto input = "3 < 5 == false;
3 > 5 == false;
1 + (2 + 3) + 4;
(5 + 5) * 2;
2 / (5 + 5);
-(5 + 5);
!(true == true);";

    const auto expected = "((3 < 5) == false);
((3 > 5) == false);
((1 + (2 + 3)) + 4);
((5 + 5) * 2);
(2 / (5 + 5));
-(5 + 5);
!(true == true);";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}
