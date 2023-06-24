module lexer

pub enum Token {
	ident
	int
	illegal
	eof
	assign
	bang
	dash
	forward_slash
	asterisk
	equal
	not_equal
	less_than
	greater_than
	plus
	comma
	semicolon
	l_paren
	r_paren
	l_squirly
	r_squirly
	function
	let
	@if // @ escapes v keywords
	@else
	@return
	@true
	@false
}

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

// next_token returns a Token and a string, which contains the tokens value
pub fn (mut l Lexer) next_token() (Token, string) {
	l.skip_whitespace()

	tok := match l.ch {
		`{` {
			Token.l_squirly
		}
		`}` {
			Token.r_squirly
		}
		`(` {
			Token.l_paren
		}
		`)` {
			Token.r_paren
		}
		`,` {
			Token.comma
		}
		`;` {
			Token.semicolon
		}
		`+` {
			Token.plus
		}
		`-` {
			Token.dash
		}
		`!` {
			if l.peek() == `=` {
				l.read_char()
				l.read_char()
				return Token.not_equal, '!='
			} else {
				Token.bang
			}
		}
		`>` {
			Token.greater_than
		}
		`<` {
			Token.less_than
		}
		`*` {
			Token.asterisk
		}
		`/` {
			Token.forward_slash
		}
		`=` {
			if l.peek() == `=` {
				l.read_char()
				l.read_char()
				return Token.equal, '=='
			} else {
				Token.assign
			}
		}
		`a`...`z`, `A`...`Z`, `_` {
			ident := l.read_ident()
			return match ident {
				'fn' { Token.function, ident }
				'let' { Token.let, ident }
				'if' { Token.@if, ident }
				'false' { Token.@false, ident }
				'true' { Token.@true, ident }
				'return' { Token.@return, ident }
				'else' { Token.@else, ident }
				else { Token.ident, ident }
			}
		}
		`0`...`9` {
			return Token.int, l.read_int()
		}
		`\0` {
			Token.eof
		}
		else {
			Token.illegal
		}
	}

	ret := l.ch.ascii_str()
	l.read_char()
	return tok, ret
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
