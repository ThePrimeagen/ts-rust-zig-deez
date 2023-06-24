module lexer

fn test_new_lexer() {
	input := 'let a = 5'
	fn_lexer := new_lexer(input)
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
	mut lexer := new_lexer(input)
	tokens := [
		Token.assign,
		Token.plus,
		Token.l_paren,
		Token.r_paren,
		Token.l_squirly,
		Token.r_squirly,
		Token.comma,
		Token.semicolon,
	]

	for token in tokens {
		next_token, _ := lexer.next_token()
		assert token == next_token
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
	mut lexer := new_lexer(input)
	tokens := [
		Token.let,
		Token.ident,
		Token.assign,
		Token.int,
		Token.semicolon,
		Token.let,
		Token.ident,
		Token.assign,
		Token.int,
		Token.semicolon,
		Token.let,
		Token.ident,
		Token.assign,
		Token.function,
		Token.l_paren,
		Token.ident,
		Token.comma,
		Token.ident,
		Token.r_paren,
		Token.l_squirly,
		Token.ident,
		Token.plus,
		Token.ident,
		Token.semicolon,
		Token.r_squirly,
		Token.semicolon,
		Token.let,
		Token.ident,
		Token.assign,
		Token.ident,
		Token.l_paren,
		Token.ident,
		Token.comma,
		Token.ident,
		Token.r_paren,
		Token.semicolon,
		Token.bang,
		Token.dash,
		Token.forward_slash,
		Token.asterisk,
		Token.int,
		Token.semicolon,
		Token.int,
		Token.less_than,
		Token.int,
		Token.greater_than,
		Token.int,
		Token.semicolon,
		Token.@if,
		Token.l_paren,
		Token.int,
		Token.less_than,
		Token.int,
		Token.r_paren,
		Token.l_squirly,
		Token.@return,
		Token.@true,
		Token.semicolon,
		Token.r_squirly,
		Token.@else,
		Token.l_squirly,
		Token.@return,
		Token.@false,
		Token.semicolon,
		Token.r_squirly,
		Token.int,
		Token.equal,
		Token.int,
		Token.semicolon,
		Token.int,
		Token.not_equal,
		Token.int,
		Token.semicolon,
		Token.eof,
	]

	for i, token in tokens {
		next_token, _ := lexer.next_token()
		assert token == next_token
	}
}
