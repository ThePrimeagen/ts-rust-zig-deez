package lexer
// Usage: `odin test .` in this folder to exec tests

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
	Equal,
	Plus,
	Comma,
	Semicolon,
	Lparen,
	Rparen,
	LSquirly,
	RSquirly,
	Function,
	Let,
	Ident,
	Number,
	EOF,
}

init_lexer :: proc(data: string) -> Lexer {
	lexer := Lexer {
		data   = transmute([]u8)data,
        tokens = make([dynamic]Token),
	}
	return lexer
}

destroy_lexer::proc(l:^Lexer) {
    delete(l.tokens)
}

scan_tokens :: proc(l: ^Lexer) {
	for !is_at_end(l) {
		scan_token(l)
	}
	append(&l.tokens, Token{offset=l.offset,kind = .EOF})
}

scan_token :: proc(l: ^Lexer) {
	c := next(l)
    start:=l.offset-1
    // fmt.print(rune(c))
	switch c {
	case '(':
		append(&l.tokens, Token{start, .Lparen, "("})
	case ')':
		append(&l.tokens, Token{start, .Rparen, ")"})
	case '{':
		append(&l.tokens, Token{start, .LSquirly, "{"})
	case '}':
		append(&l.tokens, Token{start, .RSquirly, "}"})
	case ';':
		append(&l.tokens, Token{start, .Semicolon, ":"})
    case '=':
        append(&l.tokens, Token{start, .Equal, "="})
    case '+':
        append(&l.tokens, Token{start, .Plus, "+"})
	case ',':
		append(&l.tokens, Token{start, .Comma, ","})
	case ' ', '\n', '\r', '\t': // nop
	case:
		if is_digit(c) {
			number(l)
		} else if is_alpha(c) {
            if let(l) do break
            if fn(l) do break
            ident(l)
		} else {
			append(&l.tokens, Token{start, .Illegal,""})
		}
	}
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

number :: proc(l: ^Lexer) {
	start := l.offset - 1
	for is_digit(peek(l)) do next(l)
	append(&l.tokens, Token{start, .Number, string(l.data[start:l.offset])})
}

let :: proc(l: ^Lexer) -> bool {
    start := l.offset - 1

    if l.offset + 2 >= len(l.data) || 
       l.data[l.offset] != 'e' || 
       l.data[l.offset + 1] != 't' || 
       is_alpha_numeric(l.data[l.offset + 2]) {
        return false
    }

    l.offset += 2 // skip 'e' and 't'
    append(&l.tokens, Token{start, .Let, string(l.data[start:l.offset])})

    return true
}

fn :: proc(l: ^Lexer) -> bool {
    start := l.offset - 1
    if l.offset + 1 >= len(l.data) || 
       l.data[l.offset] != 'n' || 
       is_alpha_numeric(l.data[l.offset + 1]) {
        return false
    }
    l.offset += 1 // skip 'n'
    append(&l.tokens, Token{start, .Function, string(l.data[start:l.offset])})
    return true
}

ident :: proc(l: ^Lexer) {
	start := l.offset - 1
	for is_alpha_numeric(peek(l)) do next(l)
	append(&l.tokens, Token{start, .Ident, string(l.data[start:l.offset])})
}
