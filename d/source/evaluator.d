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

import std.array : appender, Appender;
import std.conv : to;
import std.format : format;
import std.traits : isInstanceOf;

import std.range : enumerate;
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
        auto progStatements = parser.program.statements[];
        bool notReturned = true;

        for (auto i = 0; (i < progStatements.length) && notReturned; i++)
        {
            auto res = evalStatement(progStatements[i], this.parser.lexer, this.env);

            res.match!((ReturnValue _) {
                notReturned = false;
                this.results.put(res);
            }, (ErrorValue _) { notReturned = false; this.results.put(res); }, (_) {
                this.results.put(res);
            });
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

    const long[15] expected = [
        5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50
    ];

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

    const bool[8] expected = [
        true, false, false, true, false, true, false, true
    ];

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

    const bool[10] expected = [
        true, false, true, false, false, false, true, false, false, true
    ];

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

    const bool[9] expected = [
        true, true, false, true, true, true, false, false, true
    ];

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

/// If else expression statement test
unittest
{
    const auto input = "if (true) { 10 }
if (false) { 10 }
if (1) { 10 }
if (1 < 2) { 10 }
if (1 > 2) { 10 }
if (1 > 2) { 10 } else { 20 }
if (1 < 2) { 10 } else { 20 }
if (5 * 5 + 10 > 34) { 99 } else { 100 }
if ((1000 / 2) + 250 * 2 == 1000) { 9999 }";

    const long[9] expected = [10, 0, 10, 10, 0, 20, 10, 99, 9999];
    auto evaluator = prepareEvaluator(input);

    evaluator.evalProgram();

    foreach (i, result; evaluator.results[])
    {
        result.match!((bool value) => assert(value == expected[i],
                format("Bool value %s does not match expected value %s", value, expected[i])),
                (long value) => assert(value == expected[i],
                    format("Number value %s does not match expected value %s", value, expected[i])),
                (Unit _) => assert(true,
                    "Solar flare detected"),
                _ => assert(false, format("Unhandled type in statement %d", i)));
    }
}

/// Return statement test
unittest
{
    const string[4] inputs = [
        "return 10;", "return 10; 9;", "return 2 * 5; 9;", "9; return 10; 9;"
    ];

    const int[4] expected = [10, 10, 10, 10];

    for (int i = 0; i < inputs.length; i++)
    {
        auto evaluator = prepareEvaluator(inputs[i]);

        evaluator.evalProgram();

        auto result = evaluator.results[][$ - 1];

        result.match!((ReturnValue value) {
            return value.match!((long lValue) => assert(lValue == expected[i],
                format("Number value %s does not match expected value %d", lValue, expected[i])),
                _ => assert(false, format("Unhandled type in program %d", i)));
        }, _ => assert(false, format("Unhandled type in program %d", i)));
    }
}

/// Error handling statement test
unittest
{
    const string[7] inputs = [
        "5 + true;", "5 + true; 5;", "-true", "true + false", "5; true + false; 5",
        "if (10 > 1) { true + false; }",
        "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }"
    ];

    const string[7] expected = [
        "Type mismatch in expression: INTEGER + BOOLEAN",
        "Type mismatch in expression: INTEGER + BOOLEAN",
        "Unknown operator: -BOOLEAN", "Unknown operator: BOOLEAN + BOOLEAN",
        "Unknown operator: BOOLEAN + BOOLEAN",
        "Unknown operator: BOOLEAN + BOOLEAN",
        "Unknown operator: BOOLEAN + BOOLEAN"
    ];

    for (int i = 0; i < inputs.length; i++)
    {
        auto evaluator = prepareEvaluator(inputs[i]);

        evaluator.evalProgram();

        auto result = evaluator.results[][$ - 1];

        result.match!((ErrorValue value) {
            assert(value.message == expected[i],
                format("Error message %s does not match expected message %d",
                value.message, expected[i]));
        }, _ => assert(false, format("Unhandled type in program %d", i)));
    }
}
