package lexer

import (
	"testing"
)

func TestLexer(t *testing.T) {
	testCases := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Simple Tokens",
			input: "=+(){},;",
			expected: []Token{
				NewToken(Equal, "="),
				NewToken(Plus, "+"),
				NewToken(LParen, "("),
				NewToken(RParen, ")"),
				NewToken(LSquirly, "{"),
				NewToken(RSquirly, "}"),
				NewToken(Comma, ","),
				NewToken(Semicolon, ";"),
				NewToken(Eof, ""),
			},
		},
		{
			name: "Complete Token",
			input: `let five = 5;
				let ten = 10;
				let add = fn(x, y) {
					x + y;
				};
				let result = add(five, ten);`,
			expected: []Token{
				NewToken(Let, "let"),
				NewToken(Ident, "five"),
				NewToken(Equal, "="),
				NewToken(Int, "5"),
				NewToken(Semicolon, ";"),
				NewToken(Let, "let"),
				NewToken(Ident, "ten"),
				NewToken(Equal, "="),
				NewToken(Int, "10"),
				NewToken(Semicolon, ";"),
				NewToken(Let, "let"),
				NewToken(Ident, "add"),
				NewToken(Equal, "="),
				NewToken(Function, "fn"),
				NewToken(LParen, "("),
				NewToken(Ident, "x"),
				NewToken(Comma, ","),
				NewToken(Ident, "y"),
				NewToken(RParen, ")"),
				NewToken(LSquirly, "{"),
				NewToken(Ident, "x"),
				NewToken(Plus, "+"),
				NewToken(Ident, "y"),
				NewToken(Semicolon, ";"),
				NewToken(RSquirly, "}"),
				NewToken(Semicolon, ";"),
				NewToken(Let, "let"),
				NewToken(Ident, "result"),
				NewToken(Equal, "="),
				NewToken(Ident, "add"),
				NewToken(LParen, "("),
				NewToken(Ident, "five"),
				NewToken(Comma, ","),
				NewToken(Ident, "ten"),
				NewToken(RParen, ")"),
				NewToken(Semicolon, ";"),
				NewToken(Eof, ""),
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			l := NewLexer(tc.input)

			for i, expectedToken := range tc.expected {
				token := l.NextToken()
				checkToken(t, token, expectedToken, i)
			}
		})
	}
}

func checkToken(t *testing.T, token, expectedToken Token, index int) {
	if token.Type != expectedToken.Type {
		t.Errorf("Token %d - tokentype wrong. expected=%q, got=%q", index, expectedToken.Type, token.Type)
	}
	if token.Literal != expectedToken.Literal {
		t.Errorf("Token %d - literal wrong. expected=%q, got=%q", index, expectedToken.Literal, token.Literal)
	}
}
