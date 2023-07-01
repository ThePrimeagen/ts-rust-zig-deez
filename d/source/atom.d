/**
 * Object atom structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import lexer : Lexer;
import parser : BlockStatement, IdentifierNode;

import std.format : format;
import std.range : empty, enumerate;
import std.sumtype;

/// Original source of expressions: https://dlang.org/library/std/sumtype.html
/// Most fundamental type of "nothing" according to homotopy type theory
struct Unit
{
}

/// Wrapper around error message for evaluation result
struct ErrorValue
{
    string message; /// Main error message
}

/// Wrapper around error message for evaluation result
struct Function
{
    IdentifierNode[]* parameters; /// Function parameters
    BlockStatement* functionBody; /// Main function body
    Environment* env; /// Environment containing symbols
}

/// Useful for return statement
alias ReturnValue = SumType!(long, bool, string, Unit);

/// Wraps evaluation results
alias EvalResult = SumType!(long, bool, string, ReturnValue, ErrorValue, Unit, Function);

/// Describe type of atom for inspection
static const UNIT_INSTANCE = Unit(); /// Instance of null atom

static const UNIT_ATOM = EvalResult(UNIT_INSTANCE); /// Evaluation instance of null atom
static const TRUE_ATOM = EvalResult(true); /// Evaluation instance of true value
static const FALSE_ATOM = EvalResult(false); /// Evaluation instance of false value

static const VOID_RETURN_ATOM = ReturnValue(UNIT_INSTANCE); /// Return instance of void atom as return value
static const TRUE_RETURN_ATOM = ReturnValue(true); /// Return instance of true atom as return value
static const FALSE_RETURN_ATOM = ReturnValue(false); /// Return instance of false atom as return value

/// Creates new sibling environment referencing parent environment
/// Very useful for block statements
Environment* newEnclosedEnvironment(Environment* outer)
{
    auto env = new Environment();
    env.outer = outer;
    return env;
}

/// Environment extention for function
Environment* extendFunctionEnv(Function literal, EvalResult[] args, ref Lexer lexer)
{
    auto enclosedEnv = newEnclosedEnvironment(literal.env);

    foreach (paramIdx, param; (*literal.parameters).enumerate(0))
    {
        const auto id = lexer.tagRepr(param.mainIdx);
        enclosedEnv.items[id] = args[paramIdx];
    }

    return enclosedEnv;
}

/// Map out functions and variables in closures
struct Environment
{
    Environment* outer; /// Parent environment
    EvalResult[string] items; /// items in hashmap
}
