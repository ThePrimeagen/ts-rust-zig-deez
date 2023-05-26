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
				NewToken(EQUAL, "="),
				NewToken(PLUS, "+"),
				NewToken(LPAREN, "("),
				NewToken(RPAREN, ")"),
				NewToken(LSQUIRLY, "{"),
				NewToken(RSQUIRLY, "}"),
				NewToken(COMMA, ","),
				NewToken(SEMICOLON, ";"),
				NewToken(EOF, ""),
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
				NewToken(LET, "let"),
				NewToken(IDENT, "five"),
				NewToken(EQUAL, "="),
				NewToken(INT, "5"),
				NewToken(SEMICOLON, ";"),
				NewToken(LET, "let"),
				NewToken(IDENT, "ten"),
				NewToken(EQUAL, "="),
				NewToken(INT, "10"),
				NewToken(SEMICOLON, ";"),
				NewToken(LET, "let"),
				NewToken(IDENT, "add"),
				NewToken(EQUAL, "="),
				NewToken(FUNCTION, "fn"),
				NewToken(LPAREN, "("),
				NewToken(IDENT, "x"),
				NewToken(COMMA, ","),
				NewToken(IDENT, "y"),
				NewToken(RPAREN, ")"),
				NewToken(LSQUIRLY, "{"),
				NewToken(IDENT, "x"),
				NewToken(PLUS, "+"),
				NewToken(IDENT, "y"),
				NewToken(SEMICOLON, ";"),
				NewToken(RSQUIRLY, "}"),
				NewToken(SEMICOLON, ";"),
				NewToken(LET, "let"),
				NewToken(IDENT, "result"),
				NewToken(EQUAL, "="),
				NewToken(IDENT, "add"),
				NewToken(LPAREN, "("),
				NewToken(IDENT, "five"),
				NewToken(COMMA, ","),
				NewToken(IDENT, "ten"),
				NewToken(RPAREN, ")"),
				NewToken(SEMICOLON, ";"),
				NewToken(EOF, ""),
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
