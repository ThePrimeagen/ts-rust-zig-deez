package lexer

import "core:testing"
import "core:fmt"

@(test)
test_lexer :: proc(t: ^testing.T) {
	data := "=+(){},;"
	lexer := init_lexer(data)
	defer destroy_lexer(&lexer)
	scan_tokens(&lexer)

	expected_tokens := []Token{
		{0, .Assign, "="},
		{1, .Plus, "+"},
		{2, .Lparen, "("},
		{3, .Rparen, ")"},
		{4, .LSquirly, "{"},
		{5, .RSquirly, "}"},
		{6, .Comma, ","},
		{7, .Semicolon, ";"},
		{8, .EOF, ""},
	}

	for token, i in lexer.tokens {
		tokens_are_equal(lexer.tokens[i], expected_tokens[i])
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
		"let result = add(five, ten);\n"
	lexer := init_lexer(data)
	defer destroy_lexer(&lexer)
	scan_tokens(&lexer)

	expected_tokens := []Token{
		{0, .Let, "let"},
		{4, .Ident, "five"},
		{9, .Assign, "="},
		{11, .Number, "5"},
		{12, .Semicolon, ";"},
		{14, .Let, "let"},
		{18, .Ident, "ten"},
		{22, .Assign, "="},
		{24, .Number, "10"},
		{26, .Semicolon, ";"},
		{28, .Let, "let"},
		{32, .Ident, "add"},
		{36, .Assign, "="},
		{38, .Function, "fn"},
		{40, .Lparen, "("},
		{41, .Ident, "x"},
		{42, .Comma, ","},
		{44, .Ident, "y"},
		{45, .Rparen, ")"},
		{47, .LSquirly, "{"},
		{53, .Ident, "x"},
		{55, .Plus, "+"},
		{57, .Ident, "y"},
		{58, .Semicolon, ";"},
		{60, .RSquirly, "}"},
		{61, .Semicolon, ";"},
		{63, .Let, "let"},
		{67, .Ident, "result"},
		{74, .Assign, "="},
		{76, .Ident, "add"},
		{79, .Lparen, "("},
		{80, .Ident, "five"},
		{84, .Comma, ","},
		{86, .Ident, "ten"},
		{89, .Rparen, ")"},
		{90, .Semicolon, ";"},
		{92, .EOF, ""},
	}

	for token, i in lexer.tokens {
		msg, ok := tokens_are_equal(token, expected_tokens[i])
		if !ok do panic(msg)
	}
}

tokens_are_equal :: proc(a: Token, b: Token) -> (msg: string, ok: bool = true) {
	// technically could comp struct literals, but this gives more info
	if a.offset != b.offset {
		fmt.println(a, b)
		msg = "Token offset mismatch"
	}
	if a.kind != b.kind {
		fmt.println(a, b)
		msg = "Token kind mismatch"
	}
	if a.text != b.text {
		fmt.println(a, b)
		msg = "Token text mismatch"
	}
	if msg != "" {ok = false}
	return
}
