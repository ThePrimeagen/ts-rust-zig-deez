class_name Lexer

const KEYWORDS := {
	let = Token.LET, 
	fn = Token.FUNCTION,
	return = Token.RETURN,
	if = Token.IF,
	else = Token.ELSE,
	true = Token.BOOLEAN,
	false = Token.BOOLEAN
}

var input: String
var position: int

func _init(input: String) -> void:
	self.input = input

func next() -> Token:
	var token: Token
	var character: String = read_char()
	while position < input.length() and character.strip_edges().empty():
		character = read_char()
	match character:
			"-": token = Token.new(Token.DASH)
			"+": token = Token.new(Token.PLUS)
			"*": token = Token.new(Token.ASTERISK)
			":": token = Token.new(Token.COLON)
			",": token = Token.new(Token.COMMA)
			";": token = Token.new(Token.SEMICOLON)
			"(": token = Token.new(Token.OPEN_PARENTHESIS)
			")": token = Token.new(Token.CLOSE_PARENTHESIS)
			"{": token = Token.new(Token.OPEN_BRACE)
			"}": token = Token.new(Token.CLOSE_BRACE)
			"[": token = Token.new(Token.OPEN_BRACKET)
			"]": token = Token.new(Token.CLOSE_BRACKET)
			"\u0000": token = Token.new(Token.EOF)
			"/":
				if peek_char() == "/":
					skip_until("\n")
					token = next()
				else:
					token = Token.new(Token.FORWARD_SLASH)
			"=":
				if peek_char() == "=":
					position += 1
					token = Token.new(Token.EQUAL)
				else:
					token = Token.new(Token.ASSIGN)
			"!":
				if peek_char() == "=":
					position += 1
					token = Token.new(Token.NOT_EQUAL)
				else:
					token = Token.new(Token.BANG)
			">":
				if peek_char() == "=":
					position += 1
					token = Token.new(Token.GREATER_THAN_OR_EQUAL)
				else:
					token = Token.new(Token.GREATER_THAN)
			"<":
				if peek_char() == "=":
					position += 1
					token = Token.new(Token.LESS_THAN_OR_EQUAL)
				else:
					token = Token.new(Token.LESS_THAN)
			'"', "'":
				var last_position: int = position
				if skip_until(character):
					var length: int = position - last_position - 1
					token = Token.new(Token.STRING, input.substr(last_position, length))
				else:
					token = Token.new(Token.ILLEGAL)	
			_:
				var literal: String = character
				while true:
					var next_char: String = peek_char()
					if next_char.is_valid_identifier() or next_char.is_valid_integer():
						position += 1
						literal += next_char
					else:
						break
				
				if literal in KEYWORDS:
					token = Token.new(KEYWORDS[literal], literal)
				elif literal.is_valid_identifier():
					token = Token.new(Token.IDENTIFIER, literal)
				elif literal.is_valid_integer():
					token = Token.new(Token.INTEGER, literal)
				else:
					token = Token.new(Token.ILLEGAL)

	return token


func peek_char() -> String:
	return input[position] if position < input.length() else "\u0000"

func read_char() -> String:
	var character: String = peek_char()
	if ord(character):
		position += 1
	return character

func skip_until(what: String) -> bool:
	var character = read_char()
	while position < input.length() and character != what:
		character = read_char()
	return character == what
