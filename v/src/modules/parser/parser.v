module parser

import ast
import lexer
import token { Token }

struct Parser {
	l lexer.Lexer
	cur_token Token
	peek_token Token
}

fn Parser.new(l lexer.Lexer) Parser {
	p := Parser { l: l }
	p.next_token()
	p.next_token()

	return p
}

fn (p Parser) next_token() {
	p.cur_token = p.peek_token
	p.peek_token = p.l.next_token()
}

fn (p Parser) parse_program() ast.Program {
	return unsafe { nil }
}
