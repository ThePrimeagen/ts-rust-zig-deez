module lexer

import token { TokenType }

fn test_new_lexer() {
	input := 'let a = 5'
	fn_lexer := Lexer.new(input)
	// test_lexer reflects changes made by read_char() method in fn_lexer
	test_lexer := Lexer{
		position: 0
		read_position: 1
		ch: input[0]
		input: input
	}
	assert fn_lexer == test_lexer
}

fn test_next_token() {
	input := '=+(){},;'
	mut lexer := Lexer.new(input)
	tokens := [
		TokenType.assign,
		TokenType.plus,
		TokenType.l_paren,
		TokenType.r_paren,
		TokenType.l_squirly,
		TokenType.r_squirly,
		TokenType.comma,
		TokenType.semicolon,
	]

	for token in tokens {
		next_token := lexer.next_token()
		assert token == next_token.@type
	}
}

fn test_next_complete() {
	input := 'let five = 5;
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
		10 != 9;'
	mut lexer := Lexer.new(input)
	tokens := [
		TokenType.let,
		TokenType.ident,
		TokenType.assign,
		TokenType.int,
		TokenType.semicolon,
		TokenType.let,
		TokenType.ident,
		TokenType.assign,
		TokenType.int,
		TokenType.semicolon,
		TokenType.let,
		TokenType.ident,
		TokenType.assign,
		TokenType.function,
		TokenType.l_paren,
		TokenType.ident,
		TokenType.comma,
		TokenType.ident,
		TokenType.r_paren,
		TokenType.l_squirly,
		TokenType.ident,
		TokenType.plus,
		TokenType.ident,
		TokenType.semicolon,
		TokenType.r_squirly,
		TokenType.semicolon,
		TokenType.let,
		TokenType.ident,
		TokenType.assign,
		TokenType.ident,
		TokenType.l_paren,
		TokenType.ident,
		TokenType.comma,
		TokenType.ident,
		TokenType.r_paren,
		TokenType.semicolon,
		TokenType.bang,
		TokenType.minus,
		TokenType.forward_slash,
		TokenType.asterisk,
		TokenType.int,
		TokenType.semicolon,
		TokenType.int,
		TokenType.less_than,
		TokenType.int,
		TokenType.greater_than,
		TokenType.int,
		TokenType.semicolon,
		TokenType.@if,
		TokenType.l_paren,
		TokenType.int,
		TokenType.less_than,
		TokenType.int,
		TokenType.r_paren,
		TokenType.l_squirly,
		TokenType.@return,
		TokenType.@true,
		TokenType.semicolon,
		TokenType.r_squirly,
		TokenType.@else,
		TokenType.l_squirly,
		TokenType.@return,
		TokenType.@false,
		TokenType.semicolon,
		TokenType.r_squirly,
		TokenType.int,
		TokenType.equal,
		TokenType.int,
		TokenType.semicolon,
		TokenType.int,
		TokenType.not_equal,
		TokenType.int,
		TokenType.semicolon,
		TokenType.eof,
	]

	for token in tokens {
		next_token := lexer.next_token()
		assert token == next_token.@type
	}
}
