module ast

import token { Token }

interface Node {
	token_literal() string
}

interface Statement {
	Node
	statement_node()
}

interface Expression {
	Node
	expression_node()
}

struct Program {
	statements []Statement
}

fn (p Program) token_literal() string {
	if p.statements.len > 0 {
		return p.statements[0].token_literal()
	} else {
		return ''
	}
}

struct LetStatement {
	token Token
	name Identifier
	value Expression
}

fn (ls LetStatement) statement_node() {}
fn (ls LetStatement) token_literal() string {
	return ls.Token.value
}

struct Identifier {
	token Token
	value string
}

fn (i Identifier) expression_node() {}
fn (i Identifier) token_literal() string {
	return i.token.value
}
