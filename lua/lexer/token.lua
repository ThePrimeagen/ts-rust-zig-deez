---@enum TokenType
TokenType = {
	illegal = "ILLEGAL",
	eof = "EOF",

	ident = "IDENT",
	int = "INT",

	assign = "=",
	plus = "+",

	comma = ",",
	semicolon = ";",
	lparen = "(",
	rparen = ")",
	lbrace = "{",
	rbrace = "}",

	func = "FUNCTION",
	let = "LET",
}

---@type { [string]: TokenType }
Keywords = {
	["fn"] = TokenType.func,
	["let"] = TokenType.let,
}

---@class Token
---@field type TokenType
---@field literal string
Token = {}

---Creates a new Token instance with given parameters
---@param type TokenType
---@param literal string
---@return Token
function Token:new(type, literal)
	local o = {}
	setmetatable(o, self)
	self.__index = self
	self.type = type
	self.literal = literal
	return o
end

---@param ident string
---@return TokenType
function Token.lookup_ident(ident)
	local builtin = Keywords[ident]
	if builtin then
		return builtin
	end
	return TokenType.ident
end
