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

    /// Translate expression statement to result value
    EvalResult evalStatement(ExpressionStatement node)
    {
        return eval(node.expr, this.parser.lexer, this.env);
    }

    /// Evaluate the entire program
    void evalProgram()
    {
        foreach (statement; parser.program.statements[])
        {
            writefln("%s", statement.classinfo.toString);

            //if (is(typeof(statement) == ExpressionStatement))
            //if (isInstanceOf!(ExpressionStatement, statement))
            if (cast(ExpressionStatement)(statement))
            {
                auto res = this.evalStatement(cast(ExpressionStatement)(statement));
                //auto res = this.evalStatement(statement);
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
