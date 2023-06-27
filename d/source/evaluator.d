/**
 * Evaluator structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import expression;
import parser;
import statement;

import std.array : appender, Appender;
import std.conv : to;
import std.format : format;
import std.traits : isInstanceOf;

import std.sumtype : match;

/// Evaluates expressions and statement nodes
struct Evaluator
{
private:
    Parser parser; /// Stores statement nodes for evaluation
    Environment env; /// Environment for variables and definitions

public:
    Appender!(EvalResult[]) results; /// Results from each program statement

    /**
    * Constucts the evaluator.
    * Params: parser = the parser with statements to evaluate
    */
    this(ref Parser parser, ref Environment env)
    {
        this.parser = parser;
        this.env = env;
        this.results = appender!(EvalResult[]);
    }

    /// Evaluate the entire program
    void evalProgram()
    {
        foreach (statement; parser.program.statements[])
        {
            auto res = evalStatement(statement, this.parser.lexer, this.env);
            this.results.put(res);
        }
    }
}

/// Helper function to parse program before evaluating its statements/expressions
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

/// Basic integer test
unittest
{
    const auto input = "5; 10;";
    const long[2] expected = [5, 10];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((long value) => assert(value == expected[i],
                format("Number value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected int value in statement %s", i)));
    }
}

/// Compound expression statement test
unittest
{
    const auto input = "3 < 5 == false;
3 > 5 == false;
1 + (2 + 3) + 4;
(5 + 5) * 2;
2 / (5 + 5);
-(5 + 5);
!(true == true);";

    const int[7] expected = [false, true, 10, 20, 0, -10, false];
    auto evaluator = prepareEvaluator(input);

    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((bool value) => assert(value == expected[i],
                format("Bool value %s does not match expected value %s", value, expected[i])),
                (long value) => assert(value == expected[i],
                    format("Number value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Unhandled type in statement %d", i)));
    }
}
