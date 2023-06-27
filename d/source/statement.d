/**
 * Statement structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom;
import expression;
import lexer;
import parser;

import openmethods;

mixin(registerMethods);

import std.format : format;

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
    // TODO: Return
    return node.expr !is null ? eval(node.expr, lexer, env) : UNIT_ATOM;
}

///
@method EvalResult _evalStatement(BlockStatement node, Lexer lexer, Environment env)
{
    import std.stdio : writefln;

    foreach (statment; node.statements)
    {
        auto value = evalStatement(statment, lexer, env);

        // TODO: match return pattern
        writefln("Result from statment -> %s", value);
    }

    return EvalResult(VOID_RETURN_ATOM);
}
