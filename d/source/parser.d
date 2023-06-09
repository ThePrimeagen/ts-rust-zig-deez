/**
 * Parser and AST structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import lexer;
import std.array : appender, Appender;
import std.format : format;
import std.range : empty, enumerate;

import std.stdio : writefln;

/// Most fundamental node
/// TODO: use emplace for custom class allocation
class ParseNode
{
    /**
    * Interface to show the AST node.
    * Params: lexer = the lexer context for token representation
    * Returns: the string representation of the node
    */
    abstract string tokenLiteral(ref Lexer lexer);
}

/// Statement parse node
class StatementNode : ParseNode
{
}

/// Expression parse node
class ExpressionNode : ParseNode
{
}

/// Node for let statements
class LetStatement : StatementNode
{
    const ulong mainIdx; /// Main identifier index for let statement
    const ExpressionNode value; /// Expression node reference

    /**
   * Constructs let statement.
   * Params:
   * mainIdx = the index of the variable initialized
   * value = The expression initializing the variable
   */
    this(ulong mainIdx, ExpressionNode value)
    {
        this.mainIdx = mainIdx;
        this.value = value;
    }

    /// Show the let statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Node for return statements
class ReturnStatement : StatementNode
{
    const ulong mainIdx; /// Main identifier index for return statement
    const ExpressionNode value; /// Expression node reference

    /**
   * Constructs return statement.
   * Params:
   * mainIdx = the index of the statement start
   * value = The expression describing the value
   */
    this(ulong mainIdx, ExpressionNode value)
    {
        this.mainIdx = mainIdx;
        this.value = value;
    }

    /// Show the return statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Identifier node for expressions
class IdentifierNode : ExpressionNode
{
    const ulong mainIdx; /// Main identifier index

    /**
   * Constructs identifier nodes.
   * Params: mainIdx = the identifier token index
   */
    this(ulong mainIdx)
    {
        this.mainIdx = mainIdx;
    }

    /// Show the main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Integer node for expressions
class IntNode : ExpressionNode
{
    const ulong mainIdx; /// Main integer index

    /**
   * Constructs int nodes.
   * Params: mainIdx = the int token index
   */
    this(ulong mainIdx)
    {
        this.mainIdx = mainIdx;
    }

    /// Show the main integer
    override string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(mainIdx);
    }
}

/// Unary or binary operator node for expressions
class OperatorNode : ExpressionNode
{
}

/// Infix binary operator node for expressions
class InfixNode : OperatorNode
{
    const ulong operatorIdx; /// Main operator index
    ExpressionNode lhs; /// lhs expression
    ExpressionNode rhs; /// rhs expression

    /**
       * Constructs the unary/binary operator expression
       * Params:
       * operatorIdx = the index of the operator tag
       * lhs = the left hand side expression
       * rhs = the right hand side expression
       */
    this(ulong operator, ExpressionNode lhs, ExpressionNode rhs)
    {
        this.operatorIdx = operator;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    /// Show the operator for the expression
    override string tokenLiteral(ref Lexer lexer)
    {
        return lexer.tagRepr(operatorIdx);
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
    this(const Lexer lexer)
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
        if (!count)
        {
            return;
        }
        this.peekPosition += count;
        this.position = this.peekPosition;
        this.peekPosition++;
    }

    /**
     * Seeks a token ahead of the current position.
     * Returns: the token ahead of the current position.
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

    /// Parse operator for subexpression
    OperatorNode parseOperatorExpression(const ulong start)
    {
        auto lhs = new IntNode(start);
        const auto operator = this.position;

        this.skipToken();
        auto rhs = this.parseExpression();

        return new InfixNode(operator, lhs, rhs);
    }

    /// Parse expressions grouped by parentheses
    ExpressionNode parseGroupedExpression()
    {
        return null;
    }

    /// Parse expressions
    ExpressionNode parseExpression()
    {
        const auto token = tokenTags[this.position];

        with (TokenTag)
        {
            if (token == Int)
            {
                const auto start = this.position;
                const auto nextToken = this.peek();

                if (nextToken == Plus)
                {
                    this.skipToken();
                    return parseOperatorExpression(start);
                }
                else if (nextToken == Semicolon)
                {
                    this.skipToken();
                    return new IntNode(start);
                }
            }
            else if (token == LParen)
            {
                return parseGroupedExpression();
            }
        }

        return null;
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
                        tokenSlice[i], TokenTag.Let, tag));

                this.skipTokens(i);
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
                this.skipToken();
                break;
            case Eof:
                // Abort parsing on EOF
                return;
            default:
                errors.put(format("Unhandled %s token in parser", token));
                this.skipToken();
                continue;
            }

            if (statement !is null)
            {
                this.program.statements.put(statement);
            }
        }
    }
}

/** Parser tests */

/// Minimal parser test
unittest
{
    const auto input = "  ";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    assert(parser.program.statements[].empty, "Statement list must be empty for empty program");
    assert(parser.errors[].length == 0, "Error list must be empty for empty string");
}

/// Single let statement test
unittest
{
    const auto input = "let x = 5;";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    assert(parser.program.statements[].length == 1, "Statement list must not be empty for program");
    assert(parser.errors[].length == 0, "Error list must be empty for empty string");
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

    assert(parser.program.statements[].length == 3, "Statement list must not be empty for program");
    assert(parser.errors[].length == 0, "Error list must be empty for program");
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
    writefln("Got these errors -> %s", parser.errors[]);
    assert(parser.errors[].length == 3, "Error list must not be empty for erroneous program");
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

    assert(parser.program.statements[].length == 3, "Statement list must not be empty for program");
    assert(parser.errors[].length == 0, "Error list must be empty for program");
}
