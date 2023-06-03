package tests;

import haxe.Exception;
import src.Lexer;
import src.Token.TokenType;

class Main {
	static function main() {
		test1();
		test69();
	}

	static function test1() {
		final input = '=+(){},;';

		final tokens = [
			TokenType.Assign,
			TokenType.Plus,
			TokenType.LParen,
			TokenType.RParen,
			TokenType.LSquirly,
			TokenType.RSquirly,
			TokenType.Comma,
			TokenType.Semicolon,
		];

		final lexer = new Lexer(input);

		for (i in 0...tokens.length) {
			var token = lexer.nextToken();
			if (token.type != tokens[i]) {
				throw new Exception('Test1 failed on index ${i}');
			}
		}

		trace("Test1 passed!");
	}

	static function test69() {
		var input = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
    ";

		var tokens = [
			{type: TokenType.Let, literal: "let"},
			{type: TokenType.Ident, literal: "five"},
			{type: TokenType.Assign, literal: "="},
			{type: TokenType.Int, literal: "5"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Let, literal: "let"},
			{type: TokenType.Ident, literal: "ten"},
			{type: TokenType.Assign, literal: "="},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Let, literal: "let"},
			{type: TokenType.Ident, literal: "add"},
			{type: TokenType.Assign, literal: "="},
			{type: TokenType.Function, literal: "fn"},
			{type: TokenType.LParen, literal: "("},
			{type: TokenType.Ident, literal: "x"},
			{type: TokenType.Comma, literal: ","},
			{type: TokenType.Ident, literal: "y"},
			{type: TokenType.RParen, literal: ")"},
			{type: TokenType.LSquirly, literal: "{"},
			{type: TokenType.Ident, literal: "x"},
			{type: TokenType.Plus, literal: "+"},
			{type: TokenType.Ident, literal: "y"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.RSquirly, literal: "}"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Let, literal: "let"},
			{type: TokenType.Ident, literal: "result"},
			{type: TokenType.Assign, literal: "="},
			{type: TokenType.Ident, literal: "add"},
			{type: TokenType.LParen, literal: "("},
			{type: TokenType.Ident, literal: "five"},
			{type: TokenType.Comma, literal: ","},
			{type: TokenType.Ident, literal: "ten"},
			{type: TokenType.RParen, literal: ")"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Bang, literal: "!"},
			{type: TokenType.Dash, literal: "-"},
			{type: TokenType.ForwardSlash, literal: "/"},
			{type: TokenType.Asterisk, literal: "*"},
			{type: TokenType.Int, literal: "5"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Int, literal: "5"},
			{type: TokenType.LessThan, literal: "<"},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.GreaterThan, literal: ">"},
			{type: TokenType.Int, literal: "5"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.If, literal: "if"},
			{type: TokenType.LParen, literal: "("},
			{type: TokenType.Int, literal: "5"},
			{type: TokenType.LessThan, literal: "<"},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.RParen, literal: ")"},
			{type: TokenType.LSquirly, literal: "{"},
			{type: TokenType.Return, literal: "return"},
			{type: TokenType.True, literal: "true"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.RSquirly, literal: "}"},
			{type: TokenType.Else, literal: "else"},
			{type: TokenType.LSquirly, literal: "{"},
			{type: TokenType.Return, literal: "return"},
			{type: TokenType.False, literal: "false"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.RSquirly, literal: "}"},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.Equal, literal: "=="},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Int, literal: "10"},
			{type: TokenType.NotEqual, literal: "!="},
			{type: TokenType.Int, literal: "9"},
			{type: TokenType.Semicolon, literal: ";"},
			{type: TokenType.Eof, literal: "eof"},
		];

		var lexer = new Lexer(input);

		for (i in 0...tokens.length) {
			var token = lexer.nextToken();
			if (token.type != tokens[i].type && token.literal != tokens[i].literal) {
				throw new Exception('Test69 failed on index ${i}');
			}
		}

		trace("Test69 passed!");
	}
}
