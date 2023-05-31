package go_deez

import "testing"

func TestGetNextToken(t *testing.T) {
	const input = "=+(){},;"

	var tokens []TokenType

	tokens = append(tokens, Equal)
	tokens = append(tokens, Plus)
	tokens = append(tokens, LParen)
	tokens = append(tokens, RParen)
	tokens = append(tokens, LSquirly)
	tokens = append(tokens, RSquirly)
	tokens = append(tokens, Comma)
	tokens = append(tokens, Semicolon)

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

	tokens = append(tokens, CreateToken(Let, "let"))
	tokens = append(tokens, CreateToken(Ident, "five"))
	tokens = append(tokens, CreateToken(Equal, "="))
	tokens = append(tokens, CreateToken(Int, "5"))
	tokens = append(tokens, CreateToken(Semicolon, ";"))

	tokens = append(tokens, CreateToken(Let, "let"))
	tokens = append(tokens, CreateToken(Ident, "ten"))
	tokens = append(tokens, CreateToken(Equal, "="))
	tokens = append(tokens, CreateToken(Int, "10"))
	tokens = append(tokens, CreateToken(Semicolon, ";"))

	tokens = append(tokens, CreateToken(Let, "let"))
	tokens = append(tokens, CreateToken(Ident, "add"))
	tokens = append(tokens, CreateToken(Equal, "="))
	tokens = append(tokens, CreateToken(Function, "fn"))
	tokens = append(tokens, CreateToken(LParen, "("))
	tokens = append(tokens, CreateToken(Ident, "x"))
	tokens = append(tokens, CreateToken(Comma, ","))
	tokens = append(tokens, CreateToken(Ident, "y"))
	tokens = append(tokens, CreateToken(RParen, ")"))
	tokens = append(tokens, CreateToken(LSquirly, "{"))
	tokens = append(tokens, CreateToken(Ident, "x"))
	tokens = append(tokens, CreateToken(Plus, "+"))
	tokens = append(tokens, CreateToken(Ident, "y"))
	tokens = append(tokens, CreateToken(Semicolon, ";"))
	tokens = append(tokens, CreateToken(RSquirly, "}"))
	tokens = append(tokens, CreateToken(Semicolon, ";"))

	tokens = append(tokens, CreateToken(Let, "let"))
	tokens = append(tokens, CreateToken(Ident, "result"))
	tokens = append(tokens, CreateToken(Equal, "="))
	tokens = append(tokens, CreateToken(Ident, "add"))
	tokens = append(tokens, CreateToken(LParen, "("))
	tokens = append(tokens, CreateToken(Ident, "five"))
	tokens = append(tokens, CreateToken(Comma, ","))
	tokens = append(tokens, CreateToken(Ident, "ten"))
	tokens = append(tokens, CreateToken(RParen, ")"))
	tokens = append(tokens, CreateToken(Semicolon, ";"))
	tokens = append(tokens, CreateToken(Eof, "eof"))

	for _, token := range tokens {
		var nextToken = lexer.GetNextToken()
		if nextToken != token {
			t.Errorf("Token %s expected %s", nextToken, token)
		}
	}
}
