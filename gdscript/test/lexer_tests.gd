static func test_next_token() -> void:
	var input: String = "=+(){}[]<>:,;==!=>=<=$"
	
	var expected: PoolIntArray = [
		Token.ASSIGN,
		Token.PLUS,
		Token.OPEN_PARENTHESIS,
		Token.CLOSE_PARENTHESIS,
		Token.OPEN_BRACE,
		Token.CLOSE_BRACE,
		Token.OPEN_BRACKET,
		Token.CLOSE_BRACKET,
		Token.LESS_THAN,
		Token.GREATER_THAN,
		Token.COLON,
		Token.COMMA,
		Token.SEMICOLON,
		Token.EQUAL,
		Token.NOT_EQUAL,
		Token.GREATER_THAN_OR_EQUAL,
		Token.LESS_THAN_OR_EQUAL,
		Token.ILLEGAL,
		Token.EOF
	]

	_tokenize_input(input, expected)

static func test_next_token_extended() -> void:
	var input: String = """
	let five = 5;
	let ten = 10;
	let add = fn(x, y) {
		x + y;
	};

	let result = add(five, ten);
	if (result != 15) {
		return -1;
	} else {
		return true;
	}
	""".strip_edges().dedent()

	var expected: PoolIntArray = [
		Token.LET,
		Token.IDENTIFIER,
		Token.ASSIGN,
		Token.INTEGER,
		Token.SEMICOLON,
		Token.LET,
		Token.IDENTIFIER,
		Token.ASSIGN,
		Token.INTEGER,
		Token.SEMICOLON,
		Token.LET,
		Token.IDENTIFIER,
		Token.ASSIGN,
		Token.FUNCTION,
		Token.OPEN_PARENTHESIS,
		Token.IDENTIFIER,
		Token.COMMA,
		Token.IDENTIFIER,
		Token.CLOSE_PARENTHESIS,
		Token.OPEN_BRACE,
		Token.IDENTIFIER,
		Token.PLUS,
		Token.IDENTIFIER,
		Token.SEMICOLON,
		Token.CLOSE_BRACE,
		Token.SEMICOLON,
		Token.LET,
		Token.IDENTIFIER,
		Token.ASSIGN,
		Token.IDENTIFIER,
		Token.OPEN_PARENTHESIS,
		Token.IDENTIFIER,
		Token.COMMA,
		Token.IDENTIFIER,
		Token.CLOSE_PARENTHESIS,
		Token.SEMICOLON,
		Token.IF,
		Token.OPEN_PARENTHESIS,
		Token.IDENTIFIER,
		Token.NOT_EQUAL,
		Token.INTEGER,
		Token.CLOSE_PARENTHESIS,
		Token.OPEN_BRACE,
		Token.RETURN,
		Token.DASH,
		Token.INTEGER,
		Token.SEMICOLON,
		Token.CLOSE_BRACE,
		Token.ELSE,
		Token.OPEN_BRACE,
		Token.RETURN,
		Token.BOOLEAN,
		Token.SEMICOLON,
		Token.CLOSE_BRACE,
		Token.EOF
	]

	_tokenize_input(input, expected)

static func test_token_literals() -> void:
	var input: String = """
	let int = 10;
	let bool = true;
	let string = "Hello world";
	""".strip_edges().dedent()

	var expected: PoolIntArray = [
		Token.LET, Token.IDENTIFIER, Token.ASSIGN, Token.INTEGER, Token.SEMICOLON,
		Token.LET, Token.IDENTIFIER, Token.ASSIGN, Token.BOOLEAN, Token.SEMICOLON,
		Token.LET, Token.IDENTIFIER, Token.ASSIGN, Token.STRING, Token.SEMICOLON,
		Token.EOF
	]

	_tokenize_input(input, expected)

static func _tokenize_input(input: String, expected: PoolIntArray) -> void:
	var lexer := Lexer.new(input)

	for i in expected.size():
		var token: Token = lexer.next()
		var expected_type: int = expected[i]
		assert(
			token.type == expected_type,
			"Wrong token type: expected '%s', got '%s'" % [
				token.stringify_type(expected_type),
				token.stringify_type(token.type)
			]
		)

