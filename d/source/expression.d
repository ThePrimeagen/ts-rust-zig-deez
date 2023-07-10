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
import std.sumtype : match;

/// Rep of operators for expressions
/// Duplicated from Lexer, but we need to only index tag enum, not special
private const string[TokenTag.Return + 1] OPS_TAG = [
    TokenTag.Eq: "==", TokenTag.NotEq: "!=", TokenTag.Assign: "=",
    TokenTag.Plus: "+", TokenTag.Minus: "-", TokenTag.Bang: "!",
    TokenTag.Asterisk: "*", TokenTag.Slash: "/", TokenTag.Lt: "<",
    TokenTag.Gt: ">", TokenTag.Comma: ":", TokenTag.Semicolon: ";"
];

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
        }
    }

    return EvalResult(ErrorValue(format("Unknown symbol: %s", name)));
}

/// Evaluate prefix expression
@method EvalResult _eval(PrefixExpressionNode node, ref Lexer lexer, Environment* env)
{
    EvalResult result = eval(node.expr, lexer, env);

    return result.match!((bool value) {
        if ((node.op) == TokenTag.Bang) {
            return (!value) ? TRUE_ATOM : FALSE_ATOM;
        } else {
            return EvalResult(ErrorValue(format("Unknown operator: %sBOOLEAN", OPS_TAG[node.op])));
        }
    }, (long value) {
        switch (node.op) with (TokenTag) {
            static foreach (op; [Minus, Bang]) {
        case op:
                return mixin("EvalResult(" ~ OPS_TAG[op] ~ "value)");
            }
        default:
            assert(0);
        }
    }, (string _) => EvalResult("String not supported in prefix expression"),
            (ReturnValue _) => EvalResult("Return value not supported in prefix expression"),
            (ErrorValue err) => EvalResult(err), (_) => UNIT_ATOM);
}

/// Evaluate infix expression
@method EvalResult _eval(InfixExpressionNode node, ref Lexer lexer, Environment* env)
{
    const EvalResult left = eval(node.lhs, lexer, env);
    const EvalResult right = eval(node.rhs, lexer, env);

    return left.match!((bool lValue) {
        return right.match!((bool rValue) {
            switch (node.op) with (TokenTag) {
                static foreach (op; [Eq, NotEq, Gt, Lt]) {
            case op:
                    return mixin("(lValue" ~ OPS_TAG[op] ~ "rValue) ? TRUE_ATOM : FALSE_ATOM");
                }
            default:
                return EvalResult(ErrorValue(format("Unknown operator: BOOLEAN %s BOOLEAN",
                OPS_TAG[node.op])));
            }
        }, (long _) => EvalResult(ErrorValue(format("Type mismatch in expression: BOOLEAN %s INTEGER",
            OPS_TAG[node.op]))),
            _ => EvalResult(ErrorValue("Type in RHS of expression does not match BOOLEAN")));
    }, (long lValue) {
        return right.match!((bool _) => EvalResult(ErrorValue(format("Type mismatch in expression: INTEGER %s BOOLEAN",
            OPS_TAG[node.op]))), (long rValue) {
            switch (node.op) with (TokenTag) {
                static foreach (op; [Eq, NotEq, Gt, Lt]) {
            case op:
                    return mixin("(lValue" ~ OPS_TAG[op] ~ "rValue) ? TRUE_ATOM : FALSE_ATOM");
                }
                static foreach (op; [Plus, Minus, Asterisk, Slash]) {
            case op:
                    return mixin("EvalResult(lValue" ~ OPS_TAG[op] ~ "rValue)");
                }
            default:
                return EvalResult(ErrorValue(format("Unknown operator: INTEGER %s INTEGER",
                OPS_TAG[node.op])));
            }
        }, _ => EvalResult(ErrorValue("Type in RHS of expression does not match INTEGER")));
    }, (string _) => EvalResult("String not supported in infix expression"),
            (ReturnValue _) => EvalResult("Return value not supported in infix expression"),
            (ErrorValue err) => EvalResult(err), (_) => UNIT_ATOM);
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

/// Evaluate function call
@method EvalResult _eval(CallExpressionNode node, ref Lexer lexer, Environment* env)
{
    auto exprValue = eval(node.functionBody, lexer, env);

    return exprValue.match!((Function literal) {
        auto args = evalExpressions(node.args, lexer, env);

        if (args.length == 1) {
            const auto value = args[0];
            if (value.match!((ErrorValue _) => true, _ => false)) {
                return value;
            }
        }

        auto evaluated = applyFunction(literal, args, lexer);

        return evaluated.match!((ReturnValue result) {
            return result.match!((bool value) => EvalResult(value),
            (long value) => EvalResult(value),
            (string value) => EvalResult(value), (Unit _) => UNIT_ATOM);
        }, (result) => EvalResult(result));
    }, (ErrorValue error) => EvalResult(error), value => EvalResult(value));
}

/// Evaluate function with an extended environment
EvalResult applyFunction(Function literal, EvalResult[] args, ref Lexer lexer)
{
    //TODO: Add Function ADT -> Literal | Builtin
    auto extendedEnv = extendFunctionEnv(literal, args, lexer);
    return evalStatement(*literal.functionBody, lexer, extendedEnv);
}

/// Evaluate all expressions until either error or completion
EvalResult[] evalExpressions(ExpressionNode[] exprs, ref Lexer lexer, Environment* env)
{
    if (exprs.empty()) {
        return [];
    }

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
