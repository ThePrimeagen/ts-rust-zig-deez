/**
 * Object atom structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import hashable;
import lexer : Lexer;
import parser : BlockStatement, IdentifierNode;

import std.format : format;
import std.meta : AliasSeq;
import std.range : empty, enumerate;
import std.sumtype;
import std.typecons : Tuple;

/// Original source of expressions: https://dlang.org/library/std/sumtype.html
/// Most fundamental type of "nothing" according to homotopy type theory
struct Unit {
}

// Wrapper around null value
struct Nil {
    void* value;
}

/// Wrapper around error message for evaluation result
struct ErrorValue {
    string message; /// Main error message
}

/// Wrapper around Function components for evaluation result
struct Function {
    IdentifierNode[]* parameters; /// Function parameters
    BlockStatement* functionBody; /// Main function body
    Environment* env; /// Environment containing symbols
}

/// Wrapper around character
struct Character {
    char value;
}

struct BuiltinFunctionKey {
    string name;
}

/// Wraps evaluation results
alias EvalResult = SumType!(long, bool, This[], Tuple!(This, "key", This,
        "value")[HashKey], string, ErrorValue, void*, Character, Unit,
        Function, BuiltinFunctionKey, const This*);

/// Type for result array
alias Results = EvalResult.Types[2];

/// Type for result hashmap
alias ResultMap = EvalResult.Types[3];

/// Useful for return statement
alias ReturnValue = EvalResult.Types[$ - 1];

/// Mostly for logging purposes in REPL; compiled version may not have this
alias HashPair = Tuple!(EvalResult, "key", EvalResult, "value");

/// Wrapper around builtin functions for interpretation
alias BuiltinFunction = EvalResult function(EvalResult[]...);

/// Describe type of atom for inspection
static const UNIT_INSTANCE = Unit(); /// Instance of void atom
//static const NIL_INSTANCE = Nil(); /// Instance of null atom

static const EMPTY_ARRAY_ATOM = EvalResult(cast(EvalResult[])([])); /// Evaluation instance of empty array
static const UNIT_ATOM = EvalResult(UNIT_INSTANCE); /// Evaluation instance of void atom
static const NIL_ATOM = EvalResult(cast(void*) null); /// Evaluation instance of null atom
static const TRUE_ATOM = EvalResult(true); /// Evaluation instance of true value
static const FALSE_ATOM = EvalResult(false); /// Evaluation instance of false value

static const VOID_RETURN_ATOM = &UNIT_ATOM; /// Return instance of void atom as return value
//static const NIL_RETURN_ATOM = &(NIL_INSTANCE); /// Return instance of nil atom as return value
static const NIL_RETURN_ATOM = &NIL_ATOM; /// Return instance of nil atom as return value
static const TRUE_RETURN_ATOM = &TRUE_ATOM; /// Return instance of true atom as return value
static const FALSE_RETURN_ATOM = &FALSE_ATOM; /// Return instance of false atom as return value

/// Creates new sibling environment referencing parent environment
/// Very useful for block statements
Environment* newEnclosedEnvironment(Environment* outer)
{
    auto env = new Environment();
    env.outer = outer;
    return env;
}

/// Environment extension for function
Environment* extendFunctionEnv(Function literal, EvalResult[] args, ref Lexer lexer)
{
    auto enclosedEnv = newEnclosedEnvironment(literal.env);

    foreach (paramIdx, param; (*literal.parameters).enumerate(0)) {
        const auto id = param.show(lexer);
        enclosedEnv.items[id] = args[paramIdx];
    }

    return enclosedEnv;
}

/// Map out functions and variables in closures
struct Environment {
    Environment* outer; /// Parent environment
    EvalResult[string] items; /// items in hashmap
}
