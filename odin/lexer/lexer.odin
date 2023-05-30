package lexer
// Usage: `odin test .` in this folder to exec tests
// main :: proc() {
// 	// test_lexer_full(nil)
// 	data := "clear"

// 	lexer := init_lexer(data)
// 	defer destroy_lexer(&lexer)
// 	scan_tokens(&lexer)


// 	for token, i in lexer.tokens {
// 		fmt.println(token)
// 	}
// }
import "core:fmt"
Lexer :: struct {
	data:   []u8,
	offset: int,
	tokens: [dynamic]Token,
}

Token :: struct {
	offset: int,
	kind:   Token_Kind,
	text:   string,
}

Token_Kind :: enum {
	Illegal,
	EOF,
	// Idents & Literals
	Ident,
	Number,
	// Operators
	Assign,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,
	//Comparators
	LAngle,
	RAngle,
	EQ,
	Not_EQ,
	// Delimiters
	Comma,
	Semicolon,
	// Grouping
	Lparen,
	Rparen,
	LSquirly,
	RSquirly,
	// Keywords
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,
}

init_lexer :: proc(data: string) -> Lexer {
	lexer := Lexer {
		data   = transmute([]u8)data,
		tokens = make([dynamic]Token),
	}
	return lexer
}
reset_lexer :: proc(l: ^Lexer) {
	clear(&l.tokens)
	l.data = {}
	l.offset = 0
}

destroy_lexer :: proc(l: ^Lexer) {
	delete(l.tokens)
}

scan_tokens :: proc(l: ^Lexer) {
	for !is_at_end(l) {scan_token(l)}
	append(&l.tokens, Token{offset = l.offset, kind = .EOF})
}

scan_token :: proc(l: ^Lexer) {
	c := next(l)
	if is_whitespace(c) {return} 	// nop
	start := l.offset - 1
	//odinfmt: disable
	switch c {
	// Single Char:
	case '+': append(&l.tokens, Token{start, .Plus, "+"})
	case '-': append(&l.tokens, Token{start, .Minus, "-"})
	case '*': append(&l.tokens, Token{start, .Asterisk, "*"})
	case '/': append(&l.tokens, Token{start, .Slash, "/"})
	case '<': append(&l.tokens, Token{start, .LAngle, "<"})
	case '>': append(&l.tokens, Token{start, .RAngle, ">"})
	case '(': append(&l.tokens, Token{start, .Lparen, "("})
	case ')': append(&l.tokens, Token{start, .Rparen, ")"})
	case '{': append(&l.tokens, Token{start, .LSquirly, "{"})
	case '}': append(&l.tokens, Token{start, .RSquirly, "}"})
	case ';': append(&l.tokens, Token{start, .Semicolon, ";"})
	case ',': append(&l.tokens, Token{start, .Comma, ","})
	case: // Multi-Character Parsing:
		if c == '=' {
			if peek(l) == '=' {
				append(&l.tokens, Token{start, .EQ, "=="})
				next(l)
			} else {
				append(&l.tokens, Token{start, .Assign, "="})
			}
		} else if c == '!' {
			if peek(l) == '=' {
				append(&l.tokens, Token{start, .Not_EQ, "!="})
				next(l)
			} else {
				append(&l.tokens, Token{start, .Bang, "!"})
			}
		} else if try_parse_keyword(l) { /*consumed in try_parse*/
		} else if is_digit(c) {number(l)} else if is_alpha(c) {
			ident(l)
		} else {
			append(&l.tokens, Token{start, .Illegal, ""})
		}
	}
	//odinfmt: enable
}

keywords := map[Token_Kind]string {
	.If       = "if",
	.Else     = "else",
	.Function = "fn",
	.Let      = "let",
	.True     = "true",
	.False    = "false",
	.Return   = "return",
}
try_parse_keyword :: proc(l: ^Lexer) -> (parsed_ok: bool) {
	for kind, kw in keywords {
		if keyword(l, kw, kind) do return true
	}
	return false
}
is_at_end :: proc(l: ^Lexer) -> bool {
	return l.offset >= len(l.data)
}

next :: proc(l: ^Lexer) -> u8 #no_bounds_check {
	next: u8
	if l.offset < len(l.data) {
		next = l.data[l.offset]
		l.offset += 1
	}
	return next
}

peek :: proc(l: ^Lexer) -> u8 #no_bounds_check {
	if l.offset >= len(l.data) {
		return 0x0
	} else {
		return l.data[l.offset]
	}
}

is_digit :: proc(c: u8) -> bool {
	return c >= '0' && c <= '9'
}

is_alpha :: proc(c: u8) -> bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'
}

is_alpha_numeric :: proc(c: u8) -> bool {
	return is_alpha(c) || is_digit(c)
}
is_whitespace :: proc(c: u8) -> bool {
	return c == ' ' || c == '\n' || c == '\r' || c == '\t'
}
number :: proc(l: ^Lexer) {
	start := l.offset - 1
	for is_digit(peek(l)) do next(l)
	append(&l.tokens, Token{start, .Number, string(l.data[start:l.offset])})
}

keyword :: proc(l: ^Lexer, kw: string, kind: Token_Kind) -> bool {
	start := l.offset - 1
	if l.offset + len(kw) > len(l.data) {return false}
	str := string(l.data[start:start + len(kw)])
	if str == kw {
		l.offset += len(kw) - 1
		append(&l.tokens, Token{start, kind, str})
		return true
	}
	return false
}

ident :: proc(l: ^Lexer) {
	start := l.offset - 1
	for is_alpha_numeric(peek(l)) do next(l)
	str := string(l.data[start:l.offset])
	append(&l.tokens, Token{start, .Ident, str})
}
