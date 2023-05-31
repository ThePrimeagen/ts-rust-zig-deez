package go_deez

import "testing"

func TestGetNextToken(t *testing.T) {
	const input = "=+(){},;"

	var tokens []TokenType

	tokens = append(tokens, Equal, Plus, LParen, RParen,
		LSquirly, RSquirly, Comma, Semicolon)

	var lexer = NewTokenizer(input)

	for _, token := range tokens {
		var nextToken = lexer.GetNextToken()
		if nextToken.type_ != token {
			t.Errorf("Token %s expected %s", nextToken.type_, token)
		}
	}
}

func TestGetNextTokenComplete(t *testing.T) {
	const input = `let five = 5;
	let ten = 10;
	let add = fn(x, y) {
		x + y;
	};
	let result = add(five, ten);`

	var lexer = NewTokenizer(input)

	var tokens []Token

	tokens = append(tokens, CreateToken(Let, "let"),
		CreateToken(Ident, "five"),
		CreateToken(Equal, "="),
		CreateToken(Int, "5"),
		CreateToken(Semicolon, ";"),

		CreateToken(Let, "let"),
		CreateToken(Ident, "ten"),
		CreateToken(Equal, "="),
		CreateToken(Int, "10"),
		CreateToken(Semicolon, ";"),

		CreateToken(Let, "let"),
		CreateToken(Ident, "add"),
		CreateToken(Equal, "="),
		CreateToken(Function, "fn"),
		CreateToken(LParen, "("),
		CreateToken(Ident, "x"),
		CreateToken(Comma, ","),
		CreateToken(Ident, "y"),
		CreateToken(RParen, ")"),
		CreateToken(LSquirly, "{"),
		CreateToken(Ident, "x"),
		CreateToken(Plus, "+"),
		CreateToken(Ident, "y"),
		CreateToken(Semicolon, ";"),
		CreateToken(RSquirly, "}"),
		CreateToken(Semicolon, ";"),

		CreateToken(Let, "let"),
		CreateToken(Ident, "result"),
		CreateToken(Equal, "="),
		CreateToken(Ident, "add"),
		CreateToken(LParen, "("),
		CreateToken(Ident, "five"),
		CreateToken(Comma, ","),
		CreateToken(Ident, "ten"),
		CreateToken(RParen, ")"),
		CreateToken(Semicolon, ";"),
		CreateToken(Eof, "eof"))

	for _, token := range tokens {
		var nextToken = lexer.GetNextToken()
		if nextToken != token {
			t.Errorf("Token %s expected %s", nextToken, token)
		}
	}
}
