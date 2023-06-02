require("token")
require("lexer")

local function test_next_token_1()
	local input = "=+(){},;"

	---@type Token[]
	local tokens = {
		{ type = TokenType.assign, literal = "=" },
		{ type = TokenType.plus, literal = "+" },
		{ type = TokenType.lparen, literal = "(" },
		{ type = TokenType.rparen, literal = ")" },
		{ type = TokenType.lbrace, literal = "{" },
		{ type = TokenType.rbrace, literal = "}" },
		{ type = TokenType.comma, literal = "," },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.eof, literal = "" },
	}

	local l = Lexer:new(input)

	for i, tt in ipairs(tokens) do
		local tok = l:next_token()
		assert(
			tok.type == tt.type,
			string.format("tests[%d] - wrong TokenType (expected `%s`, got `%s`)", i, tt.type, tok.type)
		)
		assert(
			tok.literal == tt.literal,
			string.format("tests[%d] - wrong literal (expected `%s`, got `%s`)", i, tt.literal, tok.literal)
		)
	end
end

local function test_next_token_2()
	local input = "let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten);"

	---@type Token[]
	local tokens = {
		{ type = TokenType.let, literal = "let" },
		{ type = TokenType.ident, literal = "five" },
		{ type = TokenType.assign, literal = "=" },
		{ type = TokenType.int, literal = "5" },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.let, literal = "let" },
		{ type = TokenType.ident, literal = "ten" },
		{ type = TokenType.assign, literal = "=" },
		{ type = TokenType.int, literal = "10" },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.let, literal = "let" },
		{ type = TokenType.ident, literal = "add" },
		{ type = TokenType.assign, literal = "=" },
		{ type = TokenType.func, literal = "fn" },
		{ type = TokenType.lparen, literal = "(" },
		{ type = TokenType.ident, literal = "x" },
		{ type = TokenType.comma, literal = "," },
		{ type = TokenType.ident, literal = "y" },
		{ type = TokenType.rparen, literal = ")" },
		{ type = TokenType.lbrace, literal = "{" },
		{ type = TokenType.ident, literal = "x" },
		{ type = TokenType.plus, literal = "+" },
		{ type = TokenType.ident, literal = "y" },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.rbrace, literal = "}" },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.let, literal = "let" },
		{ type = TokenType.ident, literal = "result" },
		{ type = TokenType.assign, literal = "=" },
		{ type = TokenType.ident, literal = "add" },
		{ type = TokenType.lparen, literal = "(" },
		{ type = TokenType.ident, literal = "five" },
		{ type = TokenType.comma, literal = "," },
		{ type = TokenType.ident, literal = "ten" },
		{ type = TokenType.rparen, literal = ")" },
		{ type = TokenType.semicolon, literal = ";" },
		{ type = TokenType.eof, literal = "" },
	}

	local l = Lexer:new(input)

	for i, tt in ipairs(tokens) do
		local tok = l:next_token()
		assert(
			tok.type == tt.type,
			string.format("tests[%d] - wrong TokenType (expected `%s`, got `%s`)", i, tt.type, tok.type)
		)
		assert(
			tok.literal == tt.literal,
			string.format("tests[%d] - wrong literal (expected `%s`, got `%s`)", i, tt.literal, tok.literal)
		)
	end
end

local tests = {
	function()
		test_next_token_1()
	end,
	function()
		test_next_token_2()
	end,
}

print("Testing ---")

for i, test in ipairs(tests) do
	print(string.format("\tTest %d", i))
	test()
end

print("ALL TESTS PASSED")
