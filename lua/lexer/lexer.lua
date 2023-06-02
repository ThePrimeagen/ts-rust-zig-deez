---@class Lexer
---@field private input string
---@field private position integer
---@field private read_position integer
---@field private ch string
Lexer = {}

---Creates a new Lexer instance with given input
---@param input string
---@return Lexer
function Lexer:new(input)
	local o = {}
	setmetatable(o, self)
	self.__index = self
	self.input = input
	self.position = 1
	self.read_position = 1
	self.ch = ""
	self:read_char()
	return o
end

function Lexer:read_char()
	if self.read_position > self.input:len() then
		self.ch = ""
	else
		self.ch = self.input:sub(self.read_position, self.read_position)
	end
	self.position = self.read_position
	self.read_position = self.read_position + 1
end

---@return string
function Lexer:read_identifier()
	local position = self.position
	while self.ch:match("[a-zA-Z_]") do
		self:read_char()
	end
	return self.input:sub(position, self.position - 1)
end

---@return string
function Lexer:read_number()
	local position = self.position
	while self.ch:match("[0-9]") do
		self:read_char()
	end
	return self.input:sub(position, self.position - 1)
end

function Lexer:skip_whitespace()
	while self.ch == " " or self.ch == "\t" or self.ch == "\n" or self.ch == "\r" do
		self:read_char()
	end
end

---@return Token
function Lexer:next_token()
	local tok = {
		type = TokenType.illegal,
		literal = "ILLEGAL",
	}

	self:skip_whitespace()

	if self.ch == "=" then
		tok = Token:new(TokenType.assign, self.ch)
	elseif self.ch == ";" then
		tok = Token:new(TokenType.semicolon, self.ch)
	elseif self.ch == "(" then
		tok = Token:new(TokenType.lparen, self.ch)
	elseif self.ch == ")" then
		tok = Token:new(TokenType.rparen, self.ch)
	elseif self.ch == "," then
		tok = Token:new(TokenType.comma, self.ch)
	elseif self.ch == "+" then
		tok = Token:new(TokenType.plus, self.ch)
	elseif self.ch == "{" then
		tok = Token:new(TokenType.lbrace, self.ch)
	elseif self.ch == "}" then
		tok = Token:new(TokenType.rbrace, self.ch)
	elseif self.ch == "" then
		tok = Token:new(TokenType.eof, self.ch)
	else
		if self.ch:match("[a-zA-Z_]") then
			tok.literal = self:read_identifier()
			tok.type = Token.lookup_ident(tok.literal)
			return tok
		elseif self.ch:match("[0-9]") then
			tok.type = TokenType.int
			tok.literal = self:read_number()
			return tok
		else
			tok = Token:new(TokenType.illegal, self.ch)
		end
	end

	self:read_char()
	return tok
end
