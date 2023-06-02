package lexer
import "core:strings"
// Usage: `odin test .` in this folder to exec tests
Lexer :: struct {
	data:   []u8,
	offset: int,
	// r:      u8,
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
	t := Lexer {
		data   = transmute([]u8)data,
		offset = 0,
	}
	return t
}
scan_tokens :: proc(l: ^Lexer) {
	for !is_at_end(l) {
		scan_token(l)
	}
	append(&l.tokens, Token{offset=l.offset,kind = .EOF})
}
scan_token :: proc(t: ^Lexer) {
    skip_whitespace(t)

    start := t.offset

	switch c := next(t); c {
	case '(':
		append(&t.tokens, Token{start, .Lparen, "("})
	case ')':
		append(&t.tokens, Token{start, .Rparen, ")"})
	case '{':
		append(&t.tokens, Token{start, .LSquirly, "{"})
	case '}':
		append(&t.tokens, Token{start, .RSquirly, "}"})
	case ';':
		append(&t.tokens, Token{start, .Semicolon, ":"})
    case '=':
        append(&t.tokens, Token{start, .Equal, "="})
    case '+':
        append(&t.tokens, Token{start, .Plus, "+"})
	case ',':
		append(&t.tokens, Token{start, .Comma, ","})
    case 'a'..='z', 'A'..='Z', '_':
        read_ident(t)
        switch ident := string(t.data[start : t.offset]); ident {
        case "let":
            append(&t.tokens, Token{start, .Let, "let"})
        case "fn":
            append(&t.tokens, Token{start, .Function, "fn"})
        case:
            append(&t.tokens, Token{start, .Ident, ident})
        }
    case '0'..='9':
        read_number(t)
        append(&t.tokens, Token{start, .Number, string(t.data[start : t.offset])})
    case 0:
        append(&t.tokens, Token{start, .EOF, ""})
	case:
        append(&t.tokens, Token{start, .Illegal, ""})
	}
}
is_at_end :: proc(lexer: ^Lexer) -> bool {
	return lexer.offset >= len(lexer.data)
}
skip_whitespace :: proc(lexer: ^Lexer) {
    for strings.is_ascii_space(rune(peek(lexer))) {
        next(lexer)
    }
}
next :: proc(lexer: ^Lexer) -> u8 #no_bounds_check {
	next: u8
	if lexer.offset < len(lexer.data) {
		next = lexer.data[lexer.offset]
		lexer.offset += 1
	}
	return next
}
peek :: proc(t: ^Lexer) -> u8 #no_bounds_check {
	if t.offset >= len(t.data) {
		return 0x0
	} else {
		return t.data[t.offset]
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
//
read_number :: proc(t: ^Lexer) {
	for is_digit(peek(t)) do next(t)
}
read_ident :: proc(t: ^Lexer) {
	for is_alpha_numeric(peek(t)) do next(t)
}

import "core:testing"
import "core:fmt"

tokens_are_equal :: proc(a: Token, b: Token) {
    // technically could comp struct literals, but this gives more info
    if a.offset != b.offset{
        fmt.println(a,b)
        panic("Token offset mismatch")
    }
    if a.kind != b.kind{
        fmt.println(a,b)
        panic("Token kind mismatch")
    }
    if a.text != b.text{
        fmt.println(a,b)
        panic("Token text mismatch")
    }
}

@(test)
test_lexer :: proc(t: ^testing.T) {
    data := "=+(){},;"
    lexer := init_lexer(data);
    scan_tokens(&lexer);
    // fmt.println(lexer.tokens)

    expected_tokens := []Token{
        {0,  .Equal,    "="},
        {1,  .Plus,     "+"},
        {2,  .Lparen,   "("},
        {3,  .Rparen,   ")"},
        {4,  .LSquirly, "{"},
        {5,  .RSquirly, "}"},
        {6,  .Comma,    ","},
        {7,  .Semicolon,":"},
        {8,  .EOF,      ""},
    };

    for token,i in lexer.tokens {
        tokens_are_equal(lexer.tokens[i], expected_tokens[i]);
    }
}

@(test)
test_lexer_full :: proc(t: ^testing.T) {
    data :=
        "let five = 5;\n" +
        "let ten = 10;\n" +
        "let add = fn(x, y) {\n" +
        "    x + y;\n" +
        "};\n" +
        "let result = add(five, ten);\n";
    lexer := init_lexer(data);
    scan_tokens(&lexer);

    expected_tokens := []Token{
        {0,  .Let,      "let"},
        {4,  .Ident,    "five"},
        {9,  .Equal,    "="},
        {11, .Number,   "5"},
        {12, .Semicolon,":"},
        {14, .Let,      "let"},
        {18, .Ident,    "ten"},
        {22, .Equal,    "="},
        {24, .Number,   "10"},
        {26, .Semicolon,":"},
        {28, .Let,      "let"},
        {32, .Ident,    "add"},
        {36, .Equal,    "="},
        {38, .Function, "fn"},
        {40, .Lparen,   "("},
        {41, .Ident,    "x"},
        {42, .Comma,    ","},
        {44, .Ident,    "y"},
        {45, .Rparen,   ")"},
        {47, .LSquirly, "{"},
        {53, .Ident,    "x"},
        {55, .Plus,     "+"},
        {57, .Ident,    "y"},
        {58, .Semicolon,":"},
        {60, .RSquirly, "}"},
        {61, .Semicolon,":"},
        {63, .Let,      "let"},
        {67, .Ident,    "result"},
        {74, .Equal,    "="},
        {76, .Ident,    "add"},
        {79, .Lparen,   "("},
        {80, .Ident,    "five"},
        {84, .Comma,    ","},
        {86, .Ident,    "ten"},
        {89, .Rparen,   ")"},
        {90, .Semicolon,":"},
        {92, .EOF,      ""}
    };

    for i := 0; i < len(expected_tokens); i += 1 {
        tokens_are_equal(lexer.tokens[i], expected_tokens[i]);
    }
}
