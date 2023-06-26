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

import std.stdio : writefln;

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
    auto val = to!bool(node.show(lexer));
    writefln("Literally a bool ==> %s", val);

    return EvalResult(val);
    //return EvalResult(to!bool(node.show(lexer)));
}

///
@method EvalResult _eval(IntNode node, Lexer lexer, Environment _)
{
    return EvalResult(to!long(node.show(lexer)));
}

///
@method EvalResult _eval(IdentifierNode node, Lexer lexer, Environment env)
{
    string name = node.show(lexer);

    if (name in env.items)
    {
        return env.items[name];
    }
    else
    {
        return EvalResult(format("Unknown symbol %s", name));
    }
}

///
@method EvalResult _eval(PrefixExpressionNode node, Lexer lexer, Environment env)
{
    EvalResult result = eval(node.expr, lexer, env);

    return result.match!((bool val) {
        writefln("prefixed bool ==> %s", val);
        switch (node.op) with (TokenTag)
        {

            static foreach (op; [Minus, Bang])
            {
        case op:
                return mixin("EvalResult(" ~ OPS_TAG[op] ~ "val)");
            }
        default:
            assert(0);
        }
    }, (long val) {
        writefln("prefixed long ==> %s", val);
        switch (node.op) with (TokenTag)
        {
            static foreach (op; [Minus, Bang])
            {
        case op:
                return mixin("EvalResult(" ~ OPS_TAG[op] ~ "val)");
            }
        default:
            assert(0);
        }
    }, (string _) => EvalResult("Unhandled string prefix expression"), (Unit _) => EvalResult(
            Unit()));
}

///
@method EvalResult _eval(InfixExpressionNode node, Lexer lexer, Environment env)
{
    const EvalResult left = eval(node.lhs, lexer, env);
    const EvalResult right = eval(node.rhs, lexer, env);

    return left.match!((bool lVal) {
        return right.match!((bool rVal) {
            writefln("infixed bool ==> %s, %s", lVal, rVal);

            switch (node.op) with (TokenTag)
            {
                static foreach (op; [Eq, NotEq, Gt, Lt])
                {
            case op:
                    return mixin("EvalResult(lVal" ~ OPS_TAG[op] ~ "rVal)");
                }
            default:
                assert(0);
            }
        }, _ => EvalResult("Types in expression do not match"));
    }, (long lVal) {
        return right.match!((long rVal) {
            writefln("infixed long ==> %s, %s", lVal, rVal);

            switch (node.op) with (TokenTag)
            {
                static foreach (op; [
                    Eq, NotEq, Gt, Lt, Plus, Minus, Asterisk, Slash
                ])
                {
            case op:
                    return mixin("EvalResult(lVal" ~ OPS_TAG[op] ~ "rVal)");
                }
            default:
                assert(0);
            }
        }, _ => EvalResult("Types in expression do not match"));
    }, (string _) => EvalResult("String not supported in expression"), (Unit _) => EvalResult(
            Unit()));
}
