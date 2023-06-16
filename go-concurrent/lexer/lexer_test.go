package lexer

import (
	"testing"
)

func TestLexer(t *testing.T) {
	cases := []struct {
		input  string
		expect []token
	}{
		{
			input: `=+(){},;`,
			expect: []token{
				{tokenAssign, "="},
				{tokenPlus, "+"},
				{tokenLParen, "("},
				{tokenRParen, ")"},
				{tokenLSquirly, "{"},
				{tokenRSquirly, "}"},
				{tokenComma, ","},
				{tokenSemicolon, ";"},
				{tokenEOF, ""},
			},
		},
		{
			input: `let five = 5;
			let ten = 10;
			let add = fn(x, y) {
				x + y;
			};
			let result = add(five, ten);`,
			expect: []token{
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
				{tokenLSquirly, "{"},
				{tokenIdent, "x"},
				{tokenPlus, "+"},
				{tokenIdent, "y"},
				{tokenSemicolon, ";"},
				{tokenRSquirly, "}"},
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
			},
		},
		{
			input: `!-/*5;
			5 < 10 > 5;
			
			if (5 < 10) {
				return true;
			} else {
				return false;
			}
			
			10 == 10;
			10 != 9;`,
			expect: []token{
				{tokenBang, "!"},
				{tokenMinus, "-"},
				{tokenSlash, "/"},
				{tokenAsterisk, "*"},
				{tokenInt, "5"},
				{tokenSemicolon, ";"},
				{tokenInt, "5"},
				{tokenLessThan, "<"},
				{tokenInt, "10"},
				{tokenGreaterThan, ">"},
				{tokenInt, "5"},
				{tokenSemicolon, ";"},
				{tokenIf, "if"},
				{tokenLParen, "("},
				{tokenInt, "5"},
				{tokenLessThan, "<"},
				{tokenInt, "10"},
				{tokenRParen, ")"},
				{tokenLSquirly, "{"},
				{tokenReturn, "return"},
				{tokenTrue, "true"},
				{tokenSemicolon, ";"},
				{tokenRSquirly, "}"},
				{tokenElse, "else"},
				{tokenLSquirly, "{"},
				{tokenReturn, "return"},
				{tokenFalse, "false"},
				{tokenSemicolon, ";"},
				{tokenRSquirly, "}"},
				{tokenInt, "10"},
				{tokenEqual, "=="},
				{tokenInt, "10"},
				{tokenSemicolon, ";"},
				{tokenInt, "10"},
				{tokenNotEqual, "!="},
				{tokenInt, "9"},
				{tokenSemicolon, ";"},
				{tokenEOF, ""},
			},
		},
	}

	for _, c := range cases {
		c := c
		t.Run("", func(t *testing.T) {
			t.Parallel()

			_, tokens := NewLexer(c.input)

			for _, expectedToken := range c.expect {
				if token := <-tokens; token != expectedToken {
					t.Errorf("expected %v; got %v", expectedToken, token)
				}
			}
		})
	}
}
