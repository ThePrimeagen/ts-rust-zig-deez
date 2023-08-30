/**
 * AST macro structures and functions based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import atom : Environment;
import lexer : Lexer;
import parser;
import std.sumtype;

import openmethods;

mixin(registerMethods);

import std.range : enumerate;

/// Wrapper around ParseNode for input in modifier function
alias NodeADT = SumType!(CallExpressionNode, IdentifierNode, HashLiteralNode,
        InfixExpressionNode, PrefixExpressionNode, IndexExpressionNode, IfExpressionNode,
        FunctionLiteralNode, ArrayLiteralNode, BooleanNode, IntNode, StringNode,
        BooleanResultNode, IntResultNode, StringResultNode, ExpressionStatement,
        BlockStatement, ReturnStatement, LetStatement, ExpressionNode, StatementNode);

/// Wrapper around node modifier functions
alias NodeModifierFunction = NodeADT function(ref Lexer, Environment*, NodeADT);

/**
 * Modify underlying AST of expression for quotation or partial evaluation
 * Params:
 * node = The expression node to change
 * lexer = The lexer for showing node values
 * env = The environment storing variables
 * modifier = The node modification function
 * Returns: The final modified parse node
 */
NodeADT modify(virtual!ExpressionNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier);

/// Return modified call expression node
@method NodeADT _modify(CallExpressionNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified identifier node
@method NodeADT _modify(IdentifierNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified hash literal node
@method NodeADT _modify(HashLiteralNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    ExpressionNode[ExpressionNode] newPairs;

    foreach (keyNode, valueNode; node.pairs) {
        ExpressionNode newKey = modify(keyNode, lexer, env, modifier).match!(
                (ExpressionNode node) => node, _ => assert(false, "Unhandled key case"));

        ExpressionNode newValue = modify(valueNode, lexer, env, modifier).match!(
                (ExpressionNode node) => node, _ => assert(false, "Unhandled value case"));

        newPairs[newKey] = newValue;
    }

    node.pairs = newPairs;

    return NodeADT(node);
}

/// Return modified infix expression node
@method NodeADT _modify(InfixExpressionNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.lhs = modify(node.lhs, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled lhs case"));

    node.rhs = modify(node.rhs, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled rhs case"));

    return NodeADT(node);
}

/// Return modified prefix expression node
@method NodeADT _modify(PrefixExpressionNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.expr = modify(node.expr, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled expression case"));

    return NodeADT(node);
}

/// Return modified index expression node
@method NodeADT _modify(IndexExpressionNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.lhs = modify(node.lhs, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled lhs case"));

    node.index = modify(node.index, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false,
                "Unhandled index expression case"));

    return NodeADT(node);
}

/// Return modified if expression node
@method NodeADT _modify(IfExpressionNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    node.expr = modify(node.expr, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled expression case"));

    node.trueBranch = modifyStatement(node.trueBranch, lexer, env, modifier)
        .match!((BlockStatement node) => node, _ => assert(false, "Unhandled true branch case"));

    if (node.falseBranch !is null) {
        node.falseBranch = modifyStatement(node.falseBranch, lexer, env,
                modifier).match!((BlockStatement node) => node,
                _ => assert(false, "Unhandled false branch case"));
    }

    return NodeADT(node);
}

/// Return modified function definition node
@method NodeADT _modify(FunctionLiteralNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    foreach (i, parameter; node.parameters.enumerate(0)) {
        node.parameters[i] = modify(parameter, lexer, env, modifier)
            .match!((IdentifierNode node) => node, _ => assert(false, "Unhandled parameter case"));
    }

    node.functionBody = modifyStatement(node.functionBody, lexer, env, modifier)
        .match!((BlockStatement node) => node, _ => assert(false, "Unhandled true branch case"));

    return NodeADT(node);
}

/// Return modified array node
@method NodeADT _modify(ArrayLiteralNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    foreach (i, element; node.elements.enumerate(0)) {
        node.elements[i] = modify(element, lexer, env, modifier)
            .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled element case"));
    }

    return NodeADT(node);
}

/// Return modified boolean node
@method NodeADT _modify(BooleanNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified int node
@method NodeADT _modify(IntNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified string node
@method NodeADT _modify(StringNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified boolean node
@method NodeADT _modify(BooleanResultNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified int node
@method NodeADT _modify(IntResultNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/// Return modified string node
@method NodeADT _modify(StringResultNode node, ref Lexer lexer, Environment* env,
        NodeModifierFunction modifier)
{
    return modifier(lexer, env, NodeADT(node));
}

/**
 * Modify underlying AST of statement for quotation or partial evaluation
 * Params:
 * node = The statement node to change
 * lexer = The lexer for showing node values
 * env = The environment storing variables
 * modifier = The node modification function
 * Returns: The final modified parse node
 */
NodeADT modifyStatement(virtual!StatementNode node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier);

/// Return modified expression node
@method NodeADT _modifyStatement(ExpressionStatement node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.expr = modify(node.expr, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled expression case"));

    return NodeADT(node);
}

/// Return modified block node
@method NodeADT _modifyStatement(BlockStatement node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    foreach (i, statement; node.statements.enumerate(0)) {
        node.statements[i] = modifyStatement(statement, lexer, env, modifier)
            .match!((BlockStatement node) => node, _ => assert(false, "Unhandled statement case"));

    }

    return NodeADT(node);
}

/// Return modified return node
@method NodeADT _modifyStatement(ReturnStatement node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.expr = modify(node.expr, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled expression case"));

    return NodeADT(node);
}

/// Return modified let node
@method NodeADT _modifyStatement(LetStatement node, ref Lexer lexer,
        Environment* env, NodeModifierFunction modifier)
{
    node.expr = modify(node.expr, lexer, env, modifier)
        .match!((ExpressionNode node) => node, _ => assert(false, "Unhandled expression case"));

    return NodeADT(node);
}
