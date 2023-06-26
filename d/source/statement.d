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

EvalResult evalStatement(virtual!StatementNode node, Lexer lexer, Environment env);

/// Translate expression statement to result value
@method EvalResult _evalStatement(ExpressionStatement node, Lexer lexer, Environment env)
{
    return eval(node.expr, lexer, env);
}
