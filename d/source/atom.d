/**
 * Object atom structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.format : format;
import std.sumtype;

/// Original source of expressions: https://dlang.org/library/std/sumtype.html
/// Most fundamental type of "nothing" according to homotopy type theory
struct Unit
{
}

/// Wrapper around error message for evaluation result
struct ErrorValue
{
    string msg; /// Main error message
}

/// Useful for return statement
alias ReturnValue = SumType!(long, bool, string, Unit);

/// Wraps evaluation results
alias EvalResult = SumType!(long, bool, string, ReturnValue, ErrorValue, Unit);

/// Describe type of atom for inspection
static const UNIT_ATOM = EvalResult(Unit()); /// Instance of null atom
static const TRUE_ATOM = EvalResult(true); /// Instance of true value
static const FALSE_ATOM = EvalResult(false); /// Instance of false value

static const VOID_RETURN_ATOM = ReturnValue(Unit()); /// Instance of void atom as return value
static const TRUE_RETURN_ATOM = ReturnValue(true); /// Instance of true atom as return value
static const FALSE_RETURN_ATOM = ReturnValue(false); /// Instance of false atom as return value

/// Creates new sibling environment referencing parent environment
/// Very useful for block statements
Environment* newEnclosedEnvironment(Environment* outer)
{
    auto env = new Environment();
    env.outer = outer;
    return env;
}

/// Map out functions and variables in closures
struct Environment
{
    Environment* outer; /// Parent environment
    EvalResult[string] items; /// items in hashmap
}
