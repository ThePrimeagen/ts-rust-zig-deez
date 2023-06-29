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
        if ((node.op) == TokenTag.Bang)
        {
            return (!value) ? TRUE_ATOM : FALSE_ATOM;
        }
        else
        {
            return EvalResult(ErrorValue(format("Unknown operator: %sBOOLEAN", OPS_TAG[node.op])));
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
                return EvalResult(ErrorValue(format("Unknown operator: BOOLEAN %s BOOLEAN",
                OPS_TAG[node.op])));
            }
        }, (long _) => EvalResult(ErrorValue(format("Type mismatch in expression: BOOLEAN %s INTEGER",
            OPS_TAG[node.op]))),
            _ => EvalResult(ErrorValue("Type in RHS of expression does not match BOOLEAN")));
    }, (long lValue) {
        return right.match!((bool _) => EvalResult(ErrorValue(format("Type mismatch in expression: INTEGER %s BOOLEAN",
            OPS_TAG[node.op]))), (long rValue) {
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
                return EvalResult(ErrorValue(format("Unknown operator: INTEGER %s INTEGER",
                OPS_TAG[node.op])));
            }
        }, _ => EvalResult(ErrorValue("Type in RHS of expression does not match INTEGER")));
    }, (string _) => EvalResult("String not supported in infix expression"),
            (ReturnValue _) => EvalResult("Return value not supported in infix expression"),
            (ErrorValue err) => EvalResult(err), (Unit _) => UNIT_ATOM);
}

@method EvalResult _eval(IfExpressionNode node, Lexer lexer, Environment env)
{
    // Check value of expression
    // if expr value true ? evaluate true branch : false branch
    const EvalResult result = eval(node.expr, lexer, env);

    bool truthy = result.match!((bool value) => value, (long value) => value
            ? true : false, (string _) => true, (ReturnValue value) {
        return value.match!((bool wrappedValue) => wrappedValue,
            (long wrappedValue) => wrappedValue ? true : false, (string _) => true, _ => false);
    }, _ => false);

    if (truthy)
    {
        return evalStatement(node.trueBranch, lexer, env); /// Consequence expression
    }
    else if (node.falseBranch !is null)
    {
        return evalStatement(node.falseBranch, lexer, env); /// Alternative expression
    }
    else
    {
        return UNIT_ATOM;
    }
}

/// Translate statement to result value
EvalResult evalStatement(virtual!StatementNode node, Lexer lexer, Environment env);

///
@method EvalResult _evalStatement(ExpressionStatement node, Lexer lexer, Environment env)
{
    return eval(node.expr, lexer, env);
}

///
@method EvalResult _evalStatement(LetStatement node, Lexer lexer, Environment env)
{
    const auto value = eval(node.expr, lexer, env);
    const auto id = lexer.tagRepr(node.mainIdx);

    if (id in env.items)
    {
        // TODO: allow shadowing on interpreter extension?
        return EvalResult(ErrorValue(format("Duplicate definition of variable %s", id)));
    }
    else
    {
        env.items[id] = value;
        return value;
    }
}

///
@method EvalResult _evalStatement(ReturnStatement node, Lexer lexer, Environment env)
{
    if (node.expr !is null)
    {
        EvalResult result = eval(node.expr, lexer, env);

        return result.match!((bool value) => value ? EvalResult(TRUE_RETURN_ATOM) : EvalResult(
                FALSE_RETURN_ATOM), (long value) => EvalResult(ReturnValue(value)),
                (string value) => EvalResult(ReturnValue(value)), (ReturnValue value) => result,
                (ErrorValue value) => result, (Unit _) => EvalResult(VOID_RETURN_ATOM));

    }
    else
    {
        return EvalResult(VOID_RETURN_ATOM);
    }
}

///
@method EvalResult _evalStatement(BlockStatement node, Lexer lexer, Environment env)
{
    EvalResult value = EvalResult(VOID_RETURN_ATOM);

    bool notReturn = true;
    for (auto i = 0; (i < node.statements.length) && notReturn; i++)
    {
        value = evalStatement(node.statements[i], lexer, env);
        notReturn = value.match!((ReturnValue _) => false, (ErrorValue _) => false, _ => true);
    }

    return value;
}
