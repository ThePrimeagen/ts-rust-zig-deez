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
    const auto input = "5; 10;
-5; -10";
    const long[4] expected = [5, 10, -5, -10];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((long value) => assert(value == expected[i],
                format("Number value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected int value in statement %s", i)));
    }
}

/// More complex integer test
unittest
{
    const auto input = "5; 10; -5; -10;
5 + 5 + 5 + 5 - 10;
2 * 2 * 2 * 2 * 2;
-50 + 100 + -50;
5 * 2 + 10;
5 + 2 * 10;
20 + 2 * -10;
50 / 2 * 2 + 10;
2 * (5 + 10);
3 * 3 * 3 + 10;
3 * (3 * 3) + 10;
(5 + 10 * 2 + 15 / 3) * 2 + -10;";

    const long[15] expected = [5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((long value) => assert(value == expected[i],
                format("Number value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected int value in statement %s", i)));
    }
}

/// Basic boolean test
unittest
{
    const auto input = "true; false;
!true; !false;
!5; !!true; !!false; !!5;
";

    const bool[8] expected = [true, false, false, true, false, true, false, true];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((bool value) => assert(value == expected[i],
                format("Bool value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected bool value in statement %s", i)));
    }
}

/// Complex boolean test
unittest
{
    const auto input = "true; false;
1 < 2; 1 > 2;
1 < 1; 1 > 1;
1 == 1;
1 != 1;
1 == 2;
1 != 2;
";

    const bool[10] expected = [true, false, true, false, false, false, true, false, false, true];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((bool value) => assert(value == expected[i],
                format("Bool value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected bool value in statement %s", i)));
    }
}

/// Boolean evaluations test
unittest
{
    const auto input = "true == true;
false == false;
true == false;
true != false;
false != true;
(1 < 2) == true; (1 < 2) == false;
(1 > 2) == true; (1 > 2) == false;
";

    const bool[9] expected = [true, true, false, true, true, true, false, false, true];

    auto evaluator = prepareEvaluator(input);
    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((bool value) => assert(value == expected[i],
                format("Bool value %s does not match expected value %s", value, expected[i])),
                _ => assert(false, format("Expected bool value in statement %s", i)));
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

    const long[7] expected = [false, true, 10, 20, 0, -10, false];
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
