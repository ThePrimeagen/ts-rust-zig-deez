package lexer

import (
	"testing"
)

func TestLexerSimple(t *testing.T) {
	input := "=+(){},;"

	expect := []token{
		{tokenAssign, "="},
		{tokenPlus, "+"},
		{tokenLParen, "("},
		{tokenRParen, ")"},
		{tokenLBrace, "{"},
		{tokenRBrace, "}"},
		{tokenComma, ","},
		{tokenSemicolon, ";"},
	}

	_, tokens := NewLexer(input)

	for _, expectedToken := range expect {
		if token := <-tokens; token != expectedToken {
			t.Errorf("expected %v; got %v", expectedToken, token)
		}
	}
}

func TestLexer(t *testing.T) {
	input := `let five = 5;
	let ten = 10;
	let add = fn(x, y) {
		x + y;
	};
	let result = add(five, ten);`

	expect := []token{
		{tokenLet, "let"},
		{tokenIdent, "five"},
		{tokenAssign, "="},
		{tokenInt, "5"},
		{tokenSemicolon, ";"},
		{tokenLet, "let"},
		{tokenIdent, "ten"},
		{tokenAssign, "="},
		{tokenInt, "10"},
		{tokenSemicolon, ";"},
		{tokenLet, "let"},
		{tokenIdent, "add"},
		{tokenAssign, "="},
		{tokenFunction, "fn"},
		{tokenLParen, "("},
		{tokenIdent, "x"},
		{tokenComma, ","},
		{tokenIdent, "y"},
		{tokenRParen, ")"},
		{tokenLBrace, "{"},
		{tokenIdent, "x"},
		{tokenPlus, "+"},
		{tokenIdent, "y"},
		{tokenSemicolon, ";"},
		{tokenRBrace, "}"},
		{tokenSemicolon, ";"},
		{tokenLet, "let"},
		{tokenIdent, "result"},
		{tokenAssign, "="},
		{tokenIdent, "add"},
		{tokenLParen, "("},
		{tokenIdent, "five"},
		{tokenComma, ","},
		{tokenIdent, "ten"},
		{tokenRParen, ")"},
		{tokenSemicolon, ";"},
		{tokenEOF, ""},
	}

	_, tokens := NewLexer(input)

	for _, expectedToken := range expect {
		if token := <-tokens; token != expectedToken {
			t.Errorf("expected %v; got %v", expectedToken, token)
		}
	}
}
