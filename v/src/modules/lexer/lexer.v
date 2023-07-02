module lexer

import token { Token, TokenType }

struct Lexer {
	input string [required]
mut:
	position      usize
	read_position usize
	ch            u8
}

// new_lexer returns a Lexer containing the given input string
pub fn new_lexer(input string) Lexer {
	mut lexer := Lexer{
		input: input
	}
	lexer.read_char()

	return lexer
}

// next_token returns a Token, which contains the tokens type and value
pub fn (mut l Lexer) next_token() Token {
	l.skip_whitespace()

	tok := match l.ch {
		`{` {
			TokenType.l_squirly
		}
		`}` {
			TokenType.r_squirly
		}
		`(` {
			TokenType.l_paren
		}
		`)` {
			TokenType.r_paren
		}
		`[` {
			TokenType.l_bracket
		}
		`]` {
			TokenType.r_bracket
		}
		`,` {
			TokenType.comma
		}
		`;` {
			TokenType.semicolon
		}
		`:` {
			TokenType.colon
		}
		`+` {
			TokenType.plus
		}
		`-` {
			TokenType.minus
		}
		`!` {
			if l.peek() == `=` {
				l.read_char()
				l.read_char()
				return Token{.not_equal, '!='}
			} else {
				TokenType.bang
			}
		}
		`>` {
			TokenType.greater_than
		}
		`<` {
			TokenType.less_than
		}
		`*` {
			TokenType.asterisk
		}
		`/` {
			TokenType.forward_slash
		}
		`"` {
			return Token{.string, l.read_string()}
		}
		`=` {
			if l.peek() == `=` {
				l.read_char()
				l.read_char()
				return Token{.equal, '=='}
			} else {
				TokenType.assign
			}
		}
		`a`...`z`, `A`...`Z`, `_` {
			ident := l.read_ident()
			return match ident {
				'fn' { Token{.function, ident} }
				'let' { Token{.let, ident} }
				'true' { Token{.@true, ident} }
				'false' { Token{.@false, ident} }
				'if' { Token{.@if, ident} }
				'else' { Token{.@else, ident} }
				'return' { Token{.@return, ident} }
				else { Token{.ident, ident} }
			}
		}
		`0`...`9` {
			return Token{.int, l.read_int()}
		}
		`\0` {
			TokenType.eof
		}
		else {
			TokenType.illegal
		}
	}

	ret := l.ch.ascii_str()
	l.read_char()
	return Token{tok, ret}
}

fn (mut l Lexer) read_char() {
	if l.read_position >= l.input.len {
		l.ch = `\0`
	} else {
		l.ch = l.input[l.read_position]
	}

	l.position = l.read_position
	l.read_position++
}

fn (l Lexer) peek() u8 {
	if l.read_position >= l.input.len {
		return `\0`
	} else {
		return l.input[l.read_position]
	}
}

fn (mut l Lexer) skip_whitespace() {
	for l.ch.is_space() {
		l.read_char()
	}
}

fn (mut l Lexer) read_ident() string {
	pos := l.position
	if l.ch.is_letter() || l.ch == `_` {
		l.read_char()
	}
	for l.ch.is_alnum() || l.ch == `_` {
		l.read_char()
	}

	return l.input[pos..l.position]
}

fn (mut l Lexer) read_int() string {
	pos := l.position
	for l.ch.is_digit() {
		l.read_char()
	}

	return l.input[pos..l.position]
}

fn (mut l Lexer) read_string() string {
	l.read_char()
	pos := l.position
	for l.ch != `"` {
		l.read_char()
	}

	defer {
		l.read_char()
	}
	return l.input[pos..l.position]
}
