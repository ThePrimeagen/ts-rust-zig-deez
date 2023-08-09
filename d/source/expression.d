/**
 * Expression structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import lexer;
import parser;

import openmethods;

mixin(registerMethods);

import std.array : appender, Appender;
import std.conv : to;
import std.format : format;
import std.range : empty;
import std.stdio : writefln;
import std.sumtype : match;

/// Rep of operators for expressions
/// Duplicated from Lexer, but we need to only index tag enum, not special
private const string[TokenTag.Return + 1] OPS_TAG = [
    TokenTag.Eq: "==", TokenTag.NotEq: "!=", TokenTag.Assign: "=",
    TokenTag.Plus: "+", TokenTag.Minus: "-", TokenTag.Bang: "!",
    TokenTag.Asterisk: "*", TokenTag.Slash: "/", TokenTag.Lt: "<",
    TokenTag.Gt: ">", TokenTag.Comma: ":", TokenTag.Semicolon: ";"
];

/// Builtin functions
/// Measure length of array or string
private alias evalResult(T) = mixin("(", T, " value) => EvalResult(value)");
private alias builtinResult(T) = mixin("(", T, " value) => ReturnValue(value)");

ReturnValue len(EvalResult[] vars...)
{
    if (vars.length != 1) {
        return ReturnValue(ErrorValue(format("Wrong number of arguments. Got %d arguments, want 1 argument",
                vars.length)));
    }

    alias arrLen(T) = mixin("(", T, " value) => ReturnValue(value.length)");

    return vars[0].match!((Results value) => ReturnValue(value.length),
            arrLen!(bool[][]), arrLen!(long[][]), arrLen!(string[][]),
            arrLen!(Array!bool), arrLen!(Array!long), arrLen!(Array!string),
            arrLen!(string), (_) => ReturnValue(ErrorValue("`len` not supported for argument")));
}

/// First element of array
ReturnValue first(EvalResult[] vars...)
{
    if (vars.length != 1) {
        return ReturnValue(ErrorValue(format("Wrong number of arguments. Got %d arguments, want 1 argument",
                vars.length)));
    }

    alias firstElement(T) = mixin("(", T, " arr) { if (arr.length > 0) {
  return ReturnValue(arr[0]); } else { return NIL_RETURN_ATOM; } }");

    return vars[0].match!(firstElement!(bool[][]), firstElement!(long[][]),
            firstElement!(string[][]), firstElement!(Array!bool),
            firstElement!(Array!long), firstElement!(Array!string), (Results arr) {
        if (arr.length > 0) {
            return arr[0].match!(builtinResult!(Array!long), builtinResult!(Array!bool),
                builtinResult!(Array!string), builtinResult!(bool), builtinResult!(long),
                builtinResult!(string), builtinResult!(void*), (_) => VOID_RETURN_ATOM);
        } else {
            return NIL_RETURN_ATOM;
        }
    }, (string str) {
        if (str.length > 0) {
            return ReturnValue(Character(str[0]));
        } else {
            return ReturnValue(Character('\0'));
        }
    }, (_) => ReturnValue(ErrorValue("`first` not supported for argument")));
}

/// Last element of array
ReturnValue last(EvalResult[] vars...)
{
    if (vars.length != 1) {
        return ReturnValue(ErrorValue(format("Wrong number of arguments. Got %d arguments, want 1 argument",
                vars.length)));
    }

    alias lastElement(T) = mixin("(", T, " arr) { if (arr.length > 0) {
  return ReturnValue(arr[$ - 1]); } else { return NIL_RETURN_ATOM; } }");

    return vars[0].match!(lastElement!(bool[][]), lastElement!(long[][]),
            lastElement!(string[][]), lastElement!(Array!bool),
            lastElement!(Array!long), lastElement!(Array!string), (Results arr) {
        if (arr.length > 0) {
            return arr[$ - 1].match!(builtinResult!(Array!long), builtinResult!(Array!bool),
                builtinResult!(Array!string), builtinResult!(bool), builtinResult!(long),
                builtinResult!(string), builtinResult!(void*), (_) => VOID_RETURN_ATOM);
        } else {
            return NIL_RETURN_ATOM;
        }
    }, (string str) {
        if (str.length > 0) {
            return ReturnValue(Character(str[$ - 1]));
        } else {
            return ReturnValue(Character('\0'));
        }
    }, (_) => ReturnValue(ErrorValue("`last` not supported for argument")));
}

/// View of array tail
ReturnValue rest(EvalResult[] vars...)
{
    if (vars.length != 1) {
        return ReturnValue(ErrorValue(format("Wrong number of arguments. Got %d arguments, want 1 argument",
                vars.length)));
    }

    alias restOf(T) = mixin("(", T, " value) => ReturnValue(value[1 .. $])");

    return vars[0].match!(restOf!(bool[][]), restOf!(long[][]),
            restOf!(string[][]), restOf!(Array!bool), restOf!(Array!long),
            restOf!(Array!string), // (Results arr) {
            //        if (arr.length > 0) {
            //            return arr[1].match!((bool value) => ReturnValue(arr[1..$]),
            //                (long value) => ReturnValue(arr[1..$]), (Array!long value) => ReturnValue(arr[1..$]),
            //                (Array!bool value) => ReturnValue(arr[1..$]), (string value) => ReturnValue(arr[1..$]),
            //                (Array!string value) => ReturnValue(arr[1..$]),
            //                (void* value) => ReturnValue(arr[1..$]), (_) => ReturnValue(UNIT_INSTANCE));
            //        } else {
            //            return NIL_RETURN_ATOM;
            //        }
            //    },
            (string str) {
        if (str.length > 1) {
            return ReturnValue(str[1 .. $]);
        } else {
            return ReturnValue(Character('\0'));
        }
    }, (_) => ReturnValue(ErrorValue("`rest` not supported for argument")));
}

/// Append single value to array tail
ReturnValue push(EvalResult[] vars...)
{
    if (vars.length != 2) {
        return ReturnValue(ErrorValue(format("Wrong number of arguments. Got %d arguments, want 2 arguments",
                vars.length)));
    }

    // TODO: Generic array type would fix so many things, especially this builtin function
    alias pushValue(T) = mixin("(", T, "[] arr) {
      return vars[1].match!((", T, " value) => ReturnValue(arr ~ value),
          _ => ReturnValue(ErrorValue(\"`push` not supported for input argument\")));
    }");

    return vars[0].match!(pushValue!(bool[]), pushValue!(long[]), pushValue!(string[]),
        pushValue!(bool), pushValue!(long), pushValue!(string),
    (_) => ReturnValue(ErrorValue("argument to `push` must be array")));

  // only pattern match array - arg0
  // pattern match any old object - arg1
  // otherwise, return different error
}

/// Print variable in stdout
ReturnValue puts(EvalResult[] vars...)
{
    alias printValue(T) = mixin("(", T, " value) => writefln(\"%s\", value)");
    alias printNumber(T) = mixin("(", T, " value) => writefln(\"%d\", value)");

    // Write repr of each entry in vararg
    foreach (var; vars) {
        var.match!(printValue!(bool[][]), printValue!(long[][]),
                printValue!(string[][]), printValue!(Array!bool), printValue!(Array!long),
                printValue!(Array!string), printValue!(bool), printNumber!(long),
                printValue!(string), printValue!(void*), (_) {});
    }

    // End with unit effect
    return NIL_RETURN_ATOM;
}

immutable BuiltinFunction[string] builtinFunctions; /// Map keywords to builtin functions

shared static this()
{
    import std.exception : assumeUnique;

    BuiltinFunction[string] tempBuiltinFunctions = [
        "puts": &puts, "len": &len, "first": &first, "last": &last, "rest": &rest, "push": &push,
    ];

    builtinFunctions = assumeUnique(tempBuiltinFunctions);
}

/**
 * Translate any expression node to result value
 * Params:
 * node = The expression node to evaluate
 * lexer = The lexer for showing node values
 * env = The environment storing variables
 * Returns: The final evaluation result
 */
EvalResult eval(virtual!ExpressionNode node, ref Lexer lexer, Environment* env);

/// Simple translation of node into boolean value
@method EvalResult _eval(BooleanNode node, ref Lexer lexer, Environment* _)
{
    return EvalResult(to!bool(node.show(lexer)));
}

/// Simple translation of node into string value
@method EvalResult _eval(StringNode node, ref Lexer lexer, Environment* _)
{
    return EvalResult(node.show(lexer));
}

/// Simple translation of node into integer value
@method EvalResult _eval(IntNode node, ref Lexer lexer, Environment* _)
{
    return EvalResult(to!long(node.show(lexer)));
}

/// Search for identifier in environment before returning its value
@method EvalResult _eval(IdentifierNode node, ref Lexer lexer, Environment* env)
{
    const string name = node.show(lexer);

    for (Environment* localEnv = env; localEnv !is null; localEnv = localEnv.outer) {
        if (name in localEnv.items) {
            return localEnv.items[name];
        } else if (name in builtinFunctions) {
            return EvalResult(BuiltinFunctionKey(name));
        }
    }

    return EvalResult(ErrorValue(format("Unknown symbol: %s", name)));
}

/// Prefix expression handlers
private EvalResult boolPrefix(PrefixExpressionNode node, bool value)
{
    if ((node.op) == TokenTag.Bang) {
        return (!value) ? TRUE_ATOM : FALSE_ATOM;
    } else {
        return EvalResult(ErrorValue(format("Unknown operator: %sBOOLEAN", OPS_TAG[node.op])));
    }
}

private EvalResult longPrefix(PrefixExpressionNode node, long value)
{
    switch (node.op) with (TokenTag) {
        static foreach (op; [Minus, Bang]) {
    case op:
            return mixin("EvalResult(", OPS_TAG[op], "value)");
        }
    default:
        assert(0);
    }
}

/// Evaluate prefix expression
@method EvalResult _eval(PrefixExpressionNode node, ref Lexer lexer, Environment* env)
{
    EvalResult result = eval(node.expr, lexer, env);

    return result.match!((bool value) => boolPrefix(node, value),
            (long value) => longPrefix(node, value), (string _) => EvalResult(
                "String not supported in prefix expression"), (ReturnValue prefixValue) {
        return prefixValue.match!((bool value) => boolPrefix(node, value),
            (long value) => longPrefix(node, value),
            (string _) => EvalResult("String not supported in prefix expression"),
            (_) => EvalResult(ErrorValue("Unsupported type in LHS of infix expression")));
    }, (ErrorValue err) => EvalResult(err), (_) => UNIT_ATOM);
}

/// Infix expression handlers
private EvalResult boolInfix(InfixExpressionNode node, bool lValue, EvalResult right)
{
    return right.match!((bool rValue) {
        switch (node.op) with (TokenTag) {
            static foreach (op; [Eq, NotEq, Gt, Lt]) {
        case op:
                return mixin("(lValue", OPS_TAG[op], "rValue) ? TRUE_ATOM : FALSE_ATOM");
            }
        default:
            return EvalResult(ErrorValue(format("Unknown operator: BOOLEAN %s BOOLEAN",
                OPS_TAG[node.op])));
        }
    }, (long _) => EvalResult(ErrorValue(format("Type mismatch in expression: BOOLEAN %s INTEGER", OPS_TAG[node.op]))),
            _ => EvalResult(ErrorValue("Type in RHS of expression does not match BOOLEAN")));
}

private EvalResult longInfix(InfixExpressionNode node, long lValue, EvalResult right)
{
    return right.match!((bool _) => EvalResult(ErrorValue(format("Type mismatch in expression: INTEGER %s BOOLEAN",
            OPS_TAG[node.op]))), (long rValue) {
        switch (node.op) with (TokenTag) {
            static foreach (op; [Eq, NotEq, Gt, Lt]) {
        case op:
                return mixin("(lValue", OPS_TAG[op], "rValue) ? TRUE_ATOM : FALSE_ATOM");
            }
            static foreach (op; [Plus, Minus, Asterisk, Slash]) {
        case op:
                return mixin("EvalResult(lValue", OPS_TAG[op], "rValue)");
            }
        default:
            return EvalResult(ErrorValue(format("Unknown operator: INTEGER %s INTEGER",
                OPS_TAG[node.op])));
        }
    }, _ => EvalResult(ErrorValue("Type in RHS of expression does not match INTEGER")));
}

private EvalResult stringInfix(InfixExpressionNode node, string lValue, EvalResult right)
{
    return right.match!((string rValue) {
        if (node.op == TokenTag.Plus) {
            return EvalResult(lValue ~ rValue);
        } else {
            return EvalResult(ErrorValue(format("Unknown operator: STRING %s STRING",
                OPS_TAG[node.op])));
        }
    }, (bool _) => EvalResult(ErrorValue(format("Type mismatch in expression: STRING %s BOOLEAN",
            OPS_TAG[node.op]))), (long _) => EvalResult(ErrorValue(format(
            "Type mismatch in expression: STRING %s INTEGER", OPS_TAG[node.op]))),
            _ => EvalResult(ErrorValue("Type in RHS of expression does not match STRING")));
}

/// Evaluate infix expression
@method EvalResult _eval(InfixExpressionNode node, ref Lexer lexer, Environment* env)
{
    const EvalResult left = eval(node.lhs, lexer, env);
    const EvalResult right = eval(node.rhs, lexer, env);

    return left.match!((bool lValue) => boolInfix(node, lValue, right),
            (long lValue) => longInfix(node, lValue, right),
            (string lValue) => stringInfix(node, lValue, right), (ReturnValue lhsValue) {
        return lhsValue.match!((bool lValue) => boolInfix(node, lValue, right),
            (long lValue) => longInfix(node, lValue, right), (string lValue) => stringInfix(node, lValue, right),
            (_) => EvalResult(ErrorValue("Unsupported type in LHS of infix expression")));
    }, (ErrorValue err) => EvalResult(err), (_) => UNIT_ATOM);
}

/// Evaluate if-then-else expression
@method EvalResult _eval(IfExpressionNode node, ref Lexer lexer, Environment* env)
{
    // Check value of expression
    const EvalResult result = eval(node.expr, lexer, env);

    bool truthy = result.match!((bool value) => value, (long value) => value
            ? true : false, (string _) => true, (ReturnValue value) {
        return value.match!((bool wrappedValue) => wrappedValue,
            (long wrappedValue) => wrappedValue ? true : false, (string _) => true, _ => false);
    }, _ => false);

    // if expr value true ? evaluate true branch : false branch
    if (truthy) {
        return evalStatement(node.trueBranch, lexer, env); /// Consequence expression
    } else if (node.falseBranch !is null) {
        return evalStatement(node.falseBranch, lexer, env); /// Alternative expression
    } else {
        return UNIT_ATOM;
    }
}

/// Generate function literal from node
@method EvalResult _eval(FunctionLiteralNode node, ref Lexer lexer, Environment* env)
{
    return EvalResult(Function(&node.parameters, &node.functionBody, env));
}

/// Evaluate array literal
@method EvalResult _eval(ArrayLiteralNode node, ref Lexer lexer, Environment* env)
{
    return evalExpressionsForArray(node.elements, lexer, env);
    /*
    auto elements = evalExpressions(node.elements, lexer, env);

    if (elements.length == 1) {
        const auto value = elements[0];
        if (value.match!((ErrorValue _) => true, _ => false)) {
            return value;
        }
    }

    return EvalResult(elements);
    */
}

/// Array index expression handlers
private EvalResult longIndex(IndexExpressionNode node, EvalResult lhs, long idx,
        ref Lexer lexer, Environment* env)
{
    return lhs.match!((Results left) => evalArrayIndexExpression(left, idx, lexer, env),
            _ => EvalResult(ErrorValue("Index operator not supported for non-array on LHS")));
}

/// Evaluate array index expression
@method EvalResult _eval(IndexExpressionNode node, ref Lexer lexer, Environment* env)
{
    auto lhs = eval(node.lhs, lexer, env);
    if (lhs.match!((ErrorValue _) => true, _ => false)) {
        return lhs;
    }

    // TODO: check that LHS = hashmap
    auto index = eval(node.index, lexer, env);

    return index.match!((long idx) => longIndex(node, lhs, idx, lexer, env),
            (ErrorValue _) => index, (ReturnValue value) {
        return value.match!((long idx) => longIndex(node, lhs, idx, lexer, env),
            _ => EvalResult(ErrorValue("Index operator not supported for non-numeric on RHS")));
    }, _ => EvalResult(ErrorValue("Index operator not supported for non-numeric on RHS")));
}

EvalResult evalArrayIndexExpression(Results lhs, long index, ref Lexer lexer, Environment* env)
{
    if (index < 0 || index >= lhs.length) {
        return NIL_ATOM;
    }

    return lhs[index];
}

/// Evaluate function call
@method EvalResult _eval(CallExpressionNode node, ref Lexer lexer, Environment* env)
{
    auto exprValue = eval(node.functionBody, lexer, env);

    return exprValue.match!((BuiltinFunctionKey key) {
        auto args = evalExpressions(node.args, lexer, env);

        if (args.length == 1) {
            const auto value = args[0];
            if (value.match!((ErrorValue _) => true, _ => false)) {
                return value;
            }
        }

        return EvalResult(builtinFunctions[key.name](args));
    }, (Function literal) {
        auto args = evalExpressions(node.args, lexer, env);

        if (args.length == 1) {
            const auto value = args[0];
            if (value.match!((ErrorValue _) => true, _ => false)) {
                return value;
            }
        }

        auto evaluated = applyFunction(literal, args, lexer);

        return evaluated.match!((ReturnValue result) {
            return result.match!(evalResult!(long[][]),
            evalResult!(bool[][]), evalResult!(string[][]), evalResult!(Array!long),
            evalResult!(Array!bool), evalResult!(Array!string), evalResult!(bool),
            evalResult!(long), evalResult!(string), evalResult!(ErrorValue),
            evalResult!(void*), evalResult!(Character), (Unit _) => UNIT_ATOM);
        }, (result) => EvalResult(result));
    }, (ErrorValue error) => EvalResult(error), value => EvalResult(value));
}

/// Evaluate function with an extended environment
EvalResult applyFunction(Function literal, EvalResult[] args, ref Lexer lexer)
{
    auto extendedEnv = extendFunctionEnv(literal, args, lexer);
    return evalStatement(*literal.functionBody, lexer, extendedEnv);
}

/// Evaluate remaining expressions as array of values
EvalResult evalExpressionsArray(T)(T first, ExpressionNode[] exprs,
        ref Lexer lexer, Environment* env)
{
    if (exprs.empty()) {
        return EvalResult([first]);
    }

    auto results = appender!(T[])();
    results.reserve(exprs.length + 1);
    results.put(first);

    // Evaluate expressions until we hit error
    foreach (expression; exprs) {
        auto result = eval(expression, lexer, env);
        string errMsg = result.match!((ErrorValue err) => err.message, (T value) {
            results.put(value);
            return "";
        }, _ => "Type mismatch for array");

        if (!errMsg.empty) {
            return EvalResult(ErrorValue(errMsg));
        }
    }

    return EvalResult(results[]);
}

/// Evaluate all expressions in array until either error or completion
EvalResult evalExpressionsForArray(ExpressionNode[] exprs, ref Lexer lexer, Environment* env)
{
    if (exprs.empty()) {
        return EvalResult(cast(Results)([]));
    }

    auto firstResult = eval(exprs[0], lexer, env);
    if (firstResult.match!((ErrorValue _) => true, _ => false)) {
        return firstResult;
    }

    // FIXME: Arrays with maximum of 2 dimensions supported
    return firstResult.match!((bool value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), (long value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), (Array!long value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), (Array!bool value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), (string value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), (Array!string value) => evalExpressionsArray(value,
            exprs[1 .. $], lexer, env), //(void* _) => UNIT_ATOM,
            //(Unit _) => UNIT_ATOM,
            (_) => UNIT_ATOM);
}

/// Evaluate all expressions in param listing until either error or completion
EvalResult[] evalExpressions(ExpressionNode[] exprs, ref Lexer lexer, Environment* env)
{
    auto results = appender!(EvalResult[])();
    results.reserve(exprs.length);

    // Evaluate expressions until we hit error
    foreach (expression; exprs) {
        auto result = eval(expression, lexer, env);
        if (result.match!((ErrorValue _) => true, _ => false)) {
            return [result];
        }

        results.put(result);
    }

    return results[];
}

/**
 * Translate any statement to result value
 * Params:
 * node = The statement node to evaluate
 * lexer = The lexer for showing node values
 * env = The environment storing variables
 * Returns: The final evaluation result
 */
EvalResult evalStatement(virtual!StatementNode node, ref Lexer lexer, Environment* env);

/// Evaluate expression wrapped inside statement node
@method EvalResult _evalStatement(ExpressionStatement node, ref Lexer lexer, Environment* env)
{
    return eval(node.expr, lexer, env);
}

/// Declare new variable either globally or inside block
@method EvalResult _evalStatement(LetStatement node, ref Lexer lexer, Environment* env)
{
    const auto exprValue = eval(node.expr, lexer, env);

    return exprValue.match!((ErrorValue error) => EvalResult(error), (_) {
        const auto id = lexer.tagRepr(node.mainIdx);

        if (id in env.items) {
            // TODO: allow shadowing on interpreter extension?
            return EvalResult(ErrorValue(format("Symbol already defined: %s", id)));
        } else {
            env.items[id] = exprValue;
            return UNIT_ATOM;
        }
    });
}

/// Return value from evaluated expression inside statement
@method EvalResult _evalStatement(ReturnStatement node, ref Lexer lexer, Environment* env)
{
    if (node.expr !is null) {
        EvalResult result = eval(node.expr, lexer, env);

        return result.match!((bool value) => value ? EvalResult(TRUE_RETURN_ATOM) : EvalResult(
                FALSE_RETURN_ATOM), (long value) => EvalResult(ReturnValue(value)),
                (string value) => EvalResult(ReturnValue(value)), (ReturnValue value) => result,
                (ErrorValue value) => result, (_) => EvalResult(VOID_RETURN_ATOM));
    } else {
        return EvalResult(VOID_RETURN_ATOM);
    }
}

/// Evaluate all statements in block until return or completion
@method EvalResult _evalStatement(BlockStatement node, ref Lexer lexer, Environment* env)
{
    EvalResult value = EvalResult(VOID_RETURN_ATOM);
    bool notReturn = true;

    for (auto i = 0; (i < node.statements.length) && notReturn; i++) {
        value = evalStatement(node.statements[i], lexer, env);
        notReturn = value.match!((ReturnValue _) => false, (ErrorValue _) => false, _ => true);
    }

    return value;
}
