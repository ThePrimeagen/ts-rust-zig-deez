/**
 * Evaluator structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import parser;

import std.conv : to;
import std.format : format;
import std.traits : isInstanceOf;

import std.stdio : writefln;

/// Evaluates expressions and statement nodes
struct Evaluator
{
private:
    Parser parser; /// Stores statement nodes for evaluation
    Environment env; /// Environment for variables and definitions

public:
    /**
    * Constucts the evaluator.
    * Params: parser = the parser with statements to evaluate
    */
    this(ref Parser parser, ref Environment env)
    {
        this.parser = parser;
        this.env = env;
    }

    /// Translate any expression node to result value
    EvalResult eval(T : ExpressionNode)(ref T node)
    {
        //static if (is(typeof(node) == BooleanNode))
        static if (isInstanceOf!(BooleanNode, node))
        {
            return to!bool(node.show(this.parser.lexer));
        }
        //else static if (is(typeof(node) == IntNode))
        else static if (isInstanceOf!(IntNode, node))
        {
            return to!long(node.show(this.parser.lexer));
        }
        //else static if (is(typeof(node) == IdentifierNode))
        else static if (isInstanceOf!(IdentifierNode, node))
        {
            string name = node.show(this.parser.lexer);

            if (name in this.env.items)
            {
                return this.env.items[name];
            }
            else
            {
                return format("Unknown symbol %s", name);
            }
        }
        //else static if (is(typeof(node) == PrefixExpressionNode))
        else static if (isInstanceOf!(PrefixExpressionNode, node))
        {
            EvalResult result = eval(node.expr);

            alias valMatch = match!((long val) {
                //static if (is(typeof(node) == NegateExpressionNode))
                static if (cast(NegateExpressionNode)(node))
                    return -val;
                else
                    return 0;
            }, (bool val) {
                //static if (is(typeof(node) == BangExpressionNode))
                static if (cast(BangExpressionNode)(node))
                    return !val;
                else
                    return false;
            }, (Unit _) => Unit(), (_) => "Unhandled prefix expression");

            return valMatch(result);
        }
        //else static if (is(typeof(node) == InfixExpressionNode))
        else static if (isInstanceOf!(InfixExpressionNode, node))
        {
            EvalResult left = eval(node.lhs);
            EvalResult right = eval(node.rhs);

            alias valMatch = match!((long lVal, long rVal) {
                //static if (is(typeof(node) == PlusExpressionNode))
                static if (cast(PlusExpressionNode)(node))
                    return lVal + rVal;
                //else static if (is(typeof(node) == MinusExpressionNode))
        else static if (cast(MinusExpressionNode)(node))
                    return lVal - rVal;
                //else static if (is(typeof(node) == AsteriskExpressionNode))
        else static if (cast(AsteriskExpressionNode)(node))
                    return lVal * rVal;
                //else static if (is(typeof(node) == SlashExpressionNode))
        else static if (cast(SlashExpressionNode)(node))
                    return lVal / rVal;
                else
                    return 0;
            }, (bool lVal, bool rVal) {
                //static if (is(typeof(node) == EqExpressionNode))
                static if (cast(EqExpressionNode)(node))
                    return lVal == rVal;
                //else static if (is(typeof(node) == NotEqExpressionNode))
        else static if (cast(NotEqExpressionNode)(node))
                    return lVal != rVal;
                //else static if (is(typeof(node) == GtExpressionNode))
        else static if (cast(GtExpressionNode)(node))
                    return lVal > rVal;
                //else static if (is(typeof(node) == LtExpressionNode))
        else static if (cast(LtExpressionNode)(node))
                    return lVal < rVal;
                else
                    return false;
            }, (Unit _, Unit _) => Unit(), (_, _) => "Types in expression do not match");

            return valMatch(left, right);
        }
        else
        {
            return EvalResult("Unhandled expression");
        }
    }

    /// Translate expression statement to result value
    EvalResult evalStatement(ExpressionStatement node)
    {
        return eval(node.expr);
    }

    /// Evaluate the entire program
    void evalProgram()
    {
        foreach (statement; parser.program.statements[])
        {
            writefln("%s -> %s", statement.classinfo.toString,
                    is(typeof(statement) == ExpressionStatement));

            //if (is(typeof(statement) == ExpressionStatement))
            static if (isInstanceOf!(ExpressionStatement, statement))
            {
                //auto res = this.evalStatement(cast(ExpressionStatement)(statement));
                auto res = this.evalStatement(statement);
                writefln("LOLzy ;: %s", res);
            }
        }
    }
}

private Evaluator* prepareEvaluator(const string input)
{
    import lexer : Lexer;

    auto lexer = Lexer(input);
    lexer.tokenize();

    auto parser = Parser(lexer);
    parser.parseProgram();

    Environment env = Environment();
    return new Evaluator(parser, env);
}

unittest
{
    const auto input = "3 < 5 == false;
3 > 5 == false;
1 + (2 + 3) + 4;
(5 + 5) * 2;
2 / (5 + 5);
-(5 + 5);
!(true == true);";

    auto evaluator = prepareEvaluator(input);

    evaluator.evalProgram();
}
