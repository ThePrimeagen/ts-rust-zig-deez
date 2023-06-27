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

import std.conv : to;
import std.format : format;
import std.sumtype : match;

/// Rep of operators for expressions
/// Duplicated from Lexer, but we need to only index tag enum, not special
private const string[TokenTag.Return + 1] OPS_TAG = [
    TokenTag.Eq: "==", TokenTag.NotEq: "!=", TokenTag.Assign: "=",
    TokenTag.Plus: "+", TokenTag.Minus: "-", TokenTag.Bang: "!",
    TokenTag.Asterisk: "*", TokenTag.Slash: "/", TokenTag.Lt: "<",
    TokenTag.Gt: ">", TokenTag.Comma: ":", TokenTag.Semicolon: ";"
];

/// Translate any expression node to result value
EvalResult eval(virtual!ExpressionNode node, Lexer lexer, Environment env);

///
@method EvalResult _eval(BooleanNode node, Lexer lexer, Environment _)
{
    return EvalResult(to!bool(node.show(lexer)));
}

///
@method EvalResult _eval(IntNode node, Lexer lexer, Environment _)
{
    return EvalResult(to!long(node.show(lexer)));
}

///
@method EvalResult _eval(IdentifierNode node, Lexer lexer, Environment env)
{
    const string name = node.show(lexer);

    if (name in env.items)
    {
        return env.items[name];
    }
    else
    {
        return EvalResult(ErrorValue(format("Unknown symbol %s", name)));
    }
}

///
@method EvalResult _eval(PrefixExpressionNode node, Lexer lexer, Environment env)
{
    EvalResult result = eval(node.expr, lexer, env);

    return result.match!((bool value) {
        switch (node.op) with (TokenTag)
        {
            static foreach (op; [Minus, Bang])
            {
        case op:
                return mixin("(" ~ OPS_TAG[op] ~ "value) ? TRUE_ATOM : FALSE_ATOM");
            }
        default:
            assert(0);
        }
    }, (long value) {
        switch (node.op) with (TokenTag)
        {
            static foreach (op; [Minus, Bang])
            {
        case op:
                return mixin("EvalResult(" ~ OPS_TAG[op] ~ "value)");
            }
        default:
            assert(0);
        }
    }, (string _) => EvalResult("String not supported in prefix expression"),
            (ReturnValue _) => EvalResult("Return value not supported in prefix expression"),
            (ErrorValue err) => EvalResult(err), (Unit _) => UNIT_ATOM);
}

///
@method EvalResult _eval(InfixExpressionNode node, Lexer lexer, Environment env)
{
    const EvalResult left = eval(node.lhs, lexer, env);
    const EvalResult right = eval(node.rhs, lexer, env);

    return left.match!((bool lValue) {
        return right.match!((bool rValue) {
            switch (node.op) with (TokenTag)
            {
                static foreach (op; [Eq, NotEq, Gt, Lt])
                {
            case op:
                    return mixin("(lValue" ~ OPS_TAG[op] ~ "rValue) ? TRUE_ATOM : FALSE_ATOM");
                }
            default:
                assert(0);
            }
        }, _ => EvalResult("Types in expression do not match"));
    }, (long lValue) {
        return right.match!((long rValue) {
            switch (node.op) with (TokenTag)
            {
                static foreach (op; [Eq, NotEq, Gt, Lt])
                {
            case op:
                    return mixin("(lValue" ~ OPS_TAG[op] ~ "rValue) ? TRUE_ATOM : FALSE_ATOM");
                }
                static foreach (op; [Plus, Minus, Asterisk, Slash])
                {
            case op:
                    return mixin("EvalResult(lValue" ~ OPS_TAG[op] ~ "rValue)");
                }
            default:
                assert(0);
            }
        }, _ => EvalResult("Types in expression do not match"));
    }, (string _) => EvalResult("String not supported in infix expression"),
            (ReturnValue _) => EvalResult("Return value not supported in infix expression"),
            (ErrorValue err) => EvalResult(err), (Unit _) => UNIT_ATOM);
}
