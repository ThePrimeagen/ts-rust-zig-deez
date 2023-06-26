/**
 * Parser and AST structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import lexer;
import std.algorithm.iteration : joiner;
import std.array : appender, Appender;
import std.conv : to;
import std.format : format;
import std.range : empty, enumerate;

import openmethods;
import std.sumtype : match;

mixin(registerMethods);

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
        TokenTag.Ident: &parseIdent,
        TokenTag.Minus: &parsePrefix!(NegateExpressionNode),
        TokenTag.Bang: &parsePrefix!(BangExpressionNode), TokenTag.Int: &parseInt,
        TokenTag.True: &parseBoolean, TokenTag.False: &parseBoolean,
        TokenTag.LParen: &parseGroupedExpression, TokenTag.If: &parseIfExpression,
        TokenTag.Function: &parseFunctionLiteral
    ];

    InfixRule[TokenTag] tempInfixRules = [
        TokenTag.Plus: InfixRule(&parseBinary!(PlusExpressionNode), Precedence.Term),
        TokenTag.Minus: InfixRule(&parseBinary!(MinusExpressionNode), Precedence.Term),
        TokenTag.Asterisk: InfixRule(&parseBinary!(AsteriskExpressionNode),
                Precedence.Factor),
        TokenTag.Slash: InfixRule(&parseBinary!(SlashExpressionNode),
                Precedence.Factor),
        TokenTag.Eq: InfixRule(&parseBinary!(EqExpressionNode), Precedence.Equals),
        TokenTag.NotEq: InfixRule(&parseBinary!(NotEqExpressionNode),
                Precedence.Equals),
        TokenTag.Lt: InfixRule(&parseBinary!(LtExpressionNode), Precedence.LessGreater),
        TokenTag.Gt: InfixRule(&parseBinary!(GtExpressionNode), Precedence.LessGreater),
        TokenTag.LParen: InfixRule(&parseCallExpression, Precedence.Call),
        TokenTag.RParen: InfixRule(null, Precedence.Lowest),
        TokenTag.RSquirly: InfixRule(null, Precedence.Lowest),
        TokenTag.Comma: InfixRule(null, Precedence.Lowest),
        TokenTag.Semicolon: InfixRule(null, Precedence.Lowest),
        TokenTag.Eof: InfixRule(null, Precedence.Lowest),
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
T parsePrefix(T : PrefixExpressionNode)(ref Parser parser)
{
    const auto start = parser.position;
    parser.skipToken();

    auto expr = parser.parseExpression(Precedence.Unary);
    return new T(start, expr);
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
T parseBinary(T : InfixExpressionNode)(ref Parser parser, ref ExpressionNode lhs, Precedence prec)
{
    const auto start = parser.position;
    parser.skipToken();

    auto rhs = parser.parseExpression(cast(int)(prec) + 1);
    return new T(start, lhs, rhs);
}

/// Parse expressions grouped by parentheses
ExpressionNode parseGroupedExpression(ref Parser parser)
{
    parser.skipToken();
    auto expr = parser.parseExpression(Precedence.Lowest);

    if (parser.tokenTags[parser.position] != TokenTag.RParen)
    {
        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.RParen], parser.tokenTags[parser.position]));

        return null;
    }

    parser.skipToken();
    return expr;
}

/// Parse if-else statements
ExpressionNode parseIfExpression(ref Parser parser)
{
    const auto start = parser.position;
    const auto tokenTags = parser.tokenTags;

    // expect lparen and skip before block statement
    if (parser.peek() != TokenTag.LParen)
    {
        parser.skipToken();

        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.LParen], tokenTags[parser.position]));

        return null;
    }

    parser.skipTokens(2);
    auto expr = parser.parseExpression();

    // Expect rparen & lsquirly and skip both before consequence block statement
    if (tokenTags[parser.position] != TokenTag.RParen)
    {
        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.RParen], tokenTags[parser.position]));

        return null;
    }

    parser.skipToken();
    if (tokenTags[parser.position] != TokenTag.LSquirly)
    {
        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.LSquirly], tokenTags[parser.position]));

        return null;
    }

    auto trueBranch = parser.parseBlockStatement();

    // Expect else & lsquirly and skip both before alternative block statement
    if (tokenTags[parser.position] != TokenTag.Else)
    {
        return new IfExpressionNode(start, expr, trueBranch, null);
    }

    parser.skipToken();
    if (tokenTags[parser.position] != TokenTag.LSquirly)
    {
        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.LSquirly], tokenTags[parser.position]));

        return null;
    }

    auto falseBranch = parser.parseBlockStatement();

    return new IfExpressionNode(start, expr, trueBranch, falseBranch);
}

/// Parse function literals
ExpressionNode parseFunctionLiteral(ref Parser parser)
{
    const auto start = parser.position;
    const auto tokenTags = parser.tokenTags;

    // expect lparen and skip before function parameters
    if (parser.peek() != TokenTag.LParen)
    {
        parser.skipToken();

        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.LParen], tokenTags[parser.position]));

        return null;
    }

    parser.skipTokens(2);
    auto params = parser.parseFunctionParameters();

    // expect lsquirly and skip before block statement
    if (tokenTags[parser.position] != TokenTag.LSquirly)
    {
        parser.errors.put(format("Expected next token to be '%s'; got %s instead",
                tagReprs[TokenTag.LSquirly], tokenTags[parser.position]));

        return null;
    }

    auto functionBlock = parser.parseBlockStatement();

    return new FunctionLiteralNode(start, params, functionBlock);
}

/// Parse call expressions
ExpressionNode parseCallExpression(ref Parser parser, ref ExpressionNode functionBody, Precedence _)
{
    const auto start = parser.position;
    parser.skipToken();

    auto args = parser.parseCallArguments();

    return new CallExpressionNode(start, functionBody, args);
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

    /**
     * Evaluate the current node.
     * Params: parser = the parser context for evaluation
     * Returns: the result atom from the evaluation
     */
    //Atom eval(ref Parser parser)
    //{
    //    return null;
    //}
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

/// Translate any expression node to result value
EvalResult eval(virtual!ExpressionNode node, Lexer lexer, Environment env);

///
@method EvalResult _eval(BooleanNode node, Lexer lexer, Environment _)
{
    auto val = to!bool(node.show(lexer));
    writefln("Literally a bool ==> %s", val);

    return EvalResult(val);
    //return EvalResult(to!bool(node.show(lexer)));
}

///
@method EvalResult _eval(IntNode node, Lexer lexer, Environment _)
{
    return EvalResult(to!long(node.show(lexer)));
}

///
@method EvalResult _eval(IdentifierNode node, Lexer lexer, Environment env)
{
    string name = node.show(lexer);

    if (name in env.items)
    {
        return env.items[name];
    }
    else
    {
        return EvalResult(format("Unknown symbol %s", name));
    }
}

///
@method EvalResult _eval(PrefixExpressionNode node, Lexer lexer, Environment env)
{
    EvalResult result = eval(node.expr, lexer, env);

    return result.match!((bool val) {
        writefln("prefixed bool ==> %s", val);
        if (cast(BangExpressionNode)(node))
            return EvalResult(!val);
        else if (cast(NotEqExpressionNode)(node))
            return EvalResult(-val);
        else
            return EvalResult(false);
    }, (long val) {
        writefln("prefixed long ==> %s", val);
        if (cast(BangExpressionNode)(node))
            return EvalResult(!val);
        else if (cast(NotEqExpressionNode)(node))
            return EvalResult(-val);
        else
            return EvalResult(0);
    }, (string _) => EvalResult("Unhandled string prefix expression"), (Unit _) => EvalResult(
            Unit()));
}

///
@method EvalResult _eval(InfixExpressionNode node, Lexer lexer, Environment env)
{
    const EvalResult left = eval(node.lhs, lexer, env);
    const EvalResult right = eval(node.rhs, lexer, env);

    return left.match!((bool lVal) {
        return right.match!((bool rVal) {
            writefln("infixed bool ==> %s, %s", lVal, rVal);
            if (cast(EqExpressionNode)(node))
                return EvalResult(lVal == rVal);
            else if (cast(NotEqExpressionNode)(node))
                return EvalResult(lVal != rVal);
            else if (cast(GtExpressionNode)(node))
                return EvalResult(lVal > rVal);
            else if (cast(LtExpressionNode)(node))
                return EvalResult(lVal < rVal);
            else
                return EvalResult(false);
        }, _ => EvalResult("Types in expression do not match"));
    }, (long lVal) {
        return right.match!((long rVal) {
            writefln("infixed long ==> %s, %s", lVal, rVal);
            if (cast(EqExpressionNode)(node))
                return EvalResult(lVal == rVal);
            else if (cast(NotEqExpressionNode)(node))
                return EvalResult(lVal != rVal);
            else if (cast(GtExpressionNode)(node))
                return EvalResult(lVal > rVal);
            else if (cast(LtExpressionNode)(node))
                return EvalResult(lVal < rVal);
            else if (cast(PlusExpressionNode)(node))
                return EvalResult(lVal + rVal);
            else if (cast(MinusExpressionNode)(node))
                return EvalResult(lVal - rVal);
            else if (cast(AsteriskExpressionNode)(node))
                return EvalResult(lVal * rVal);
            else if (cast(SlashExpressionNode)(node))
                return EvalResult(lVal / rVal);
            else
                return EvalResult(0);
        }, _ => EvalResult("Types in expression do not match"));
    }, (string _) => EvalResult("String not supported in expression"), (Unit _) => EvalResult(
            Unit()));
}

/// Wrapper for ExpressionNodes
class ExpressionStatement : StatementNode
{
    ExpressionNode expr; /// Expression node reference

    /**
     * Constructs expression statement.
     * Params:
     * mainIdx = the starting index of the expression
     * expr = The main expression creating the value
     */
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx);
        this.expr = expr;
    }

    override string show(ref Lexer lexer)
    {
        return (this.expr !is null) ? expr.show(lexer) ~ ";" : "";
    }
}

/// Node for let statements
class LetStatement : StatementNode
{
    ExpressionNode expr; /// Expression node reference

    /**
     * Constructs let statement.
     * Params:
     * mainIdx = the index of the variable initialized
     * expr = The main expression creating the value
     */
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx);
        this.expr = expr;
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
                (this.expr !is null) ? this.expr.show(lexer) : "");
    }
}

/// Node for return statements
class ReturnStatement : StatementNode
{
    ExpressionNode expr; /// Expression node reference

    /**
     * Constructs return statement.
     * Params:
     * mainIdx = the index of the statement start
     * expr = The main expression creating the value
     */
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx);
        this.expr = expr;
    }

    /// Show the return statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Return);
    }

    /// Create statement string
    override string show(ref Lexer lexer)
    {
        return format("return%s;", (this.expr !is null) ? ' ' ~ this.expr.show(lexer) : "");
    }
}

/// Node for nesting statements in blocks
class BlockStatement : StatementNode
{
    StatementNode[] statements; /// Statement listing in block

    /**
     * Constructs return statement.
     * Params:
     * mainIdx = the index of the block start
     * statements = The statement listing
     */
    this(ulong mainIdx, StatementNode[] statements)
    {
        super(mainIdx);
        this.statements = statements;
    }

    /// Show the block statement's main identifier
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.LSquirly);
    }

    /// Create statement string
    override string show(ref Lexer lexer)
    {
        auto reprBuilder = appender!(string[]);
        reprBuilder.reserve(8);

        foreach (statement; this.statements[])
        {
            auto repr = statement.show(lexer);
            if (repr !is null && repr != "")
            {
                reprBuilder.put(repr);
            }
        }

        const auto blockRepr = reprBuilder[].joiner("\n").to!string;
        return (blockRepr != "") ? "{ " ~ blockRepr ~ " }" : "{}";
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

    /// Represent integer literal value
    //override Atom eval(ref Parser parser)
    //{
    //    const auto val = parser.getInt64Value(mainIdx);
    //    return new IntegerAtom(val);
    //}
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

/// Prefix expression specialized for '-'
class NegateExpressionNode : PrefixExpressionNode
{
    /// Constructs prefix expression for '-'
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx, expr);
    }
}

/// Prefix expression specialized for '!'
class BangExpressionNode : PrefixExpressionNode
{
    /// Constructs prefix expression for '!'
    this(ulong mainIdx, ref ExpressionNode expr)
    {
        super(mainIdx, expr);
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

/// Infix expression specialized for '+'
class PlusExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '+'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '-'
class MinusExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '-'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '*'
class AsteriskExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '*'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '/'
class SlashExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '/'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '=='
class EqExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '=='
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '!='
class NotEqExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '!='
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '<'
class LtExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '<'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Infix expression specialized for '>'
class GtExpressionNode : InfixExpressionNode
{
    /// Constructs infix expression for '>'
    this(ulong mainIdx, ref ExpressionNode lhs, ref ExpressionNode rhs)
    {
        super(mainIdx, lhs, rhs);
    }
}

/// Expression node for ifs
class IfExpressionNode : ExpressionNode
{
    ExpressionNode expr; /// Main expression to choose consequence/alternative
    BlockStatement trueBranch; /// Consequence expression
    BlockStatement falseBranch; /// Alternative expression

    /**
     * Constructs the if expression
     * Params:
     * mainIdx = the index of the operator tag
     * expr = the main expression
     * trueBranch = the expression for the 'true' branch
     * falseBranch = the expression for the 'false' branch
     */
    this(ulong mainIdx, ExpressionNode expr, BlockStatement trueBranch, BlockStatement falseBranch)
    {
        super(mainIdx);
        this.expr = expr;
        this.trueBranch = trueBranch;
        this.falseBranch = falseBranch;
    }

    /// Show the if keyword
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.If);
    }

    /// Create expression string
    override string show(ref Lexer lexer)
    {
        return format("if %s %s%s", (this.expr !is null) ? expr.show(lexer) : "",
                (this.trueBranch !is null) ? trueBranch.show(lexer) : "",
                (this.falseBranch !is null) ? " else " ~ falseBranch.show(lexer) : "");
    }
}

/// Expression node for function literals
class FunctionLiteralNode : ExpressionNode
{
    IdentifierNode[] parameters; /// Function parameters
    BlockStatement functionBody; /// Main function body

    /**
     * Constructs the function literal expression
     * Params:
     * mainIdx = the index of the operator tag
     * parameters = the function parameters
     * functionBody = the body of the function
     */
    this(ulong mainIdx, IdentifierNode[] parameters, ref BlockStatement functionBody)
    {
        super(mainIdx);
        this.parameters = parameters;
        this.functionBody = functionBody;
    }

    /// Show the if keyword
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.Function);
    }

    /// Create expression string
    override string show(ref Lexer lexer)
    {
        string paramListing = "";
        if (this.parameters !is null && this.parameters.length)
        {
            auto paramsBuilder = appender!(string[]);
            paramsBuilder.reserve(this.parameters.length);

            foreach (param; this.parameters)
            {
                auto repr = param.show(lexer);
                if (repr !is null && repr != "")
                {
                    paramsBuilder.put(repr);
                }
            }

            paramListing = paramsBuilder[].joiner(", ").to!string;
        }

        return format("fn(%s) %s", paramListing, (this.functionBody !is null)
                ? functionBody.show(lexer) : "{}");
    }
}

/// Expression node for calls
class CallExpressionNode : ExpressionNode
{
    ExpressionNode functionBody; /// Ident | FunctionLiteral
    ExpressionNode[] args; /// Arguments for call expression

    /**
     * Constructs the binary operator expression
     * Params:
     * mainIdx = the index of the operator tag
     * functionBody = the main function body
     * args = the expressions as parameter inputs
     */
    this(ulong mainIdx, ExpressionNode functionBody, ExpressionNode[] args)
    {
        super(mainIdx);
        this.functionBody = functionBody;
        this.args = args;
    }

    /// Show the left parenthesis
    override string tokenLiteral(ref Lexer lexer)
    {
        return to!string(TokenTag.LParen);
    }

    /// Create call expression string
    override string show(ref Lexer lexer)
    {
        string argsListing = "";
        if (!this.args.empty())
        {
            auto argsBuilder = appender!(string[]);
            argsBuilder.reserve(this.args.length);

            foreach (arg; this.args)
            {
                auto repr = arg.show(lexer);
                if (repr !is null && repr != "")
                {
                    argsBuilder.put(repr);
                }
            }

            argsListing = argsBuilder[].joiner(", ").to!string;
        }

        return format("%s(%s)", (this.functionBody !is null)
                ? functionBody.show(lexer) : "", argsListing);
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
        auto statementList = this.statements[];

        auto reprBuilder = appender!(string[]);
        reprBuilder.reserve(statementList.length);

        foreach (statement; statementList)
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
    const TokenTag[] tokenTags; /// Alias for tokens in lexer
    const ulong tokenCount; /// Cached token length

public:
    Lexer lexer; /// Lexer instance with token list
    Appender!(string[]) errors; /// List of parse errors
    Program program; /// Container for program statements

    /**
     * Constructs the parser.
     * Params: lexer = the lexer with tokens to parse
     */
    this(ref Lexer lexer)
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
     * Seek ahead a certain number of tokens in the parser.
     * Params: count = the number of tokens to seek towards
     */
    TokenTag seekToken(ulong count)
    {
        if (count < 1)
        {
            return this.tokenTags[this.position];
        }

        const auto seekPosition = this.position + count;
        if (seekPosition < this.tokenCount)
        {
            return this.tokenTags[seekPosition];
        }
        else
        {
            return TokenTag.Eof;
        }
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
            this.errors.put(format("Expected expression, but got %s instead", token));
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
                this.errors.put(format("Invalid token %s for infix expression", token));
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

        if (sliceEnd > tokenCount)
        {
            this.errors.put("Not enough tokens for Let statement");
            return null;
        }

        const auto tokenSlice = tokenTags[start .. sliceEnd];

        foreach (i, tag; expectedTags.enumerate(0))
        {
            if (tag != tokenSlice[i])
            {
                this.errors.put(format("Expected next token to be '%s'; got %s instead",
                        (tag in tagReprs) ? tagReprs[tag] : to!string(tag), tokenSlice[i]));

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

        // Parse expression as part of return statement
        auto value = this.parseExpression();

        if (tokenTags[this.position] == TokenTag.Semicolon)
        {
            this.skipToken();
        }

        return new ReturnStatement(start, value);
    }

    /// Parse block statements
    BlockStatement parseBlockStatement()
    {
        const auto start = this.position;
        this.skipToken();

        auto blockBuilder = appender!(StatementNode[])();
        blockBuilder.reserve(8);

        // Keep parsing statements until block ends
        while (this.position < this.tokenCount && this.tokenTags[this.position] != TokenTag
                .RSquirly)
        {
            auto statement = this.parseStatement();
            if (statement !is null)
            {
                blockBuilder.put(statement);
            }
        }

        this.skipToken();
        return new BlockStatement(start, blockBuilder[]);
    }

    /// Parse statements
    StatementNode parseStatement()
    {
        const auto token = this.tokenTags[this.position];
        switch (token) with (TokenTag)
        {
        case Let:
            return this.parseLetStatement();
        case Return:
            return this.parseReturnStatement();
        case Semicolon:
            goto case; // Explicit fallthrough to EOF case
        case Eof:
            this.skipToken();
            return null;
        default:
            return this.parseExpressionStatement();
        }
    }

    /// Parse function parameter comma separated list
    IdentifierNode[] parseFunctionParameters()
    {
        auto paramsBuilder = appender!(IdentifierNode[])();
        TokenTag token = this.tokenTags[this.position];

        while (this.position < this.tokenCount)
        {
            // Expect rparen at end of paren list
            if (token == TokenTag.RParen)
            {
                this.skipToken();
                return paramsBuilder[];
            }

            // Get next identifier if possible
            if (token == TokenTag.Ident)
            {
                paramsBuilder.put(new IdentifierNode(this.position));
            }
            else
            {
                this.errors.put(format("Expected next token to be identifier; got %s instead",
                        token));

                return null;
            }

            const auto nextToken = this.peek();
            if (nextToken == TokenTag.Comma)
            {
                token = this.seekToken(2);
                this.skipTokens(2);
            }
            else
            {
                token = nextToken;
                this.skipToken();
            }
        }

        // Escape hatch for invalid parameter list
        return null;
    }

    /// Parse parameter input comma separated list
    ExpressionNode[] parseCallArguments()
    {
        auto argsBuilder = appender!(ExpressionNode[])();
        TokenTag token = this.tokenTags[this.position];

        while (this.position < this.tokenCount)
        {
            // Expect rparen at end of paren list
            if (token == TokenTag.RParen)
            {
                this.skipToken();
                return argsBuilder[];
            }

            // Get next expression if possible
            auto expr = this.parseExpression(Precedence.Lowest);
            argsBuilder.put(expr);

            const auto nextToken = this.tokenTags[this.position];
            if (nextToken == TokenTag.Comma)
            {
                token = this.peek();
                this.skipToken();
            }
            else
            {
                token = nextToken;
            }
        }

        // Escape hatch for invalid parameter list
        return null;
    }

    /// Parse token list to create statement AST nodes
    void parseProgram()
    {
        while (this.position < this.tokenCount)
        {
            StatementNode statement = parseStatement();

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

/// Simple if expression test
unittest
{
    const auto input = "if (x < y) { x }";
    const auto expected = "if (x < y) { x; };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Simple if-else expression test
unittest
{
    const auto input = "if (x < y) { x } else { y }";
    const auto expected = "if (x < y) { x; } else { y; };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// More complicated if-else expression test
unittest
{
    const auto input = "if ((x * 7) < 5 - y) { x + 4 } else { y - 2 }";
    const auto expected = "if ((x * 7) < (5 - y)) { (x + 4); } else { (y - 2); };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function without parameters test
unittest
{
    const auto input = "fn() { return foobar + barfoo; }";
    const auto expected = "fn() { return (foobar + barfoo); };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function literal test
unittest
{
    const auto input = "fn(x, y) { return x + y; }";
    const auto expected = "fn(x, y) { return (x + y); };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function returning function test
unittest
{
    const auto input = "fn() { return fn(x, y) { return x + y; }}";
    const auto expected = "fn() { return fn(x, y) { return (x + y); }; };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function as variable test
unittest
{
    const auto input = "let myFunction = fn(x, y) { return x + y; }";
    const auto expected = "let myFunction = fn(x, y) { return (x + y); };";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function parameter test
unittest
{
    const auto input = "fn() {};
fn(x) {};
fn(x, y, z) {};";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(input, lexer, parser);
}

/// Function as function parameter test
unittest
{
    const auto input = "myFunc(x, y, fn(x, y) { return x + y; });";
    const auto expected = "myFunc(x, y, fn(x, y) { return (x + y); });";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Function def and call parameter test
unittest
{
    const auto input = "fn(x, y) { x + y; }(2, 3);";
    const auto expected = "fn(x, y) { (x + y); }(2, 3);";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}

/// Call expression test
unittest
{
    const auto input = "add(1, 2 * 3, 4 + 5);
a + add(b * c) + d;
add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
add(a + b + c * d / f + g);";

    const auto expected = "add(1, (2 * 3), (4 + 5));
((a + add((b * c))) + d);
add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));
add((((a + b) + ((c * d) / f)) + g));";

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    validateParseProgram(expected, lexer, parser);
}
