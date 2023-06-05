class_name Token

enum {
	IDENTIFIER,

	ASSIGN,
	BANG,
	DASH,
	PLUS,
	ASTERISK,
	EQUAL,
	NOT_EQUAL,
	GREATER_THAN,
	LESS_THAN,
	GREATER_THAN_OR_EQUAL,
	LESS_THAN_OR_EQUAL,
	FORWARD_SLASH,
	OPEN_PARENTHESIS,
	CLOSE_PARENTHESIS,
	OPEN_BRACE,
	CLOSE_BRACE,
	OPEN_BRACKET,
	CLOSE_BRACKET,
	COLON,
	COMMA,
	SEMICOLON,
	
	LET,
	FUNCTION,
	RETURN,

	IF,
	ELSE,

	BOOLEAN,
	INTEGER,
	STRING,

	ILLEGAL,
	EOF
}

var type: int
var literal: String

func _init(token_type: int, token_literal: String = "") -> void:
	type = token_type
	literal = token_literal

func as_int() -> int:
	if type != INTEGER:
		_print_type_error(INTEGER)

	# String#to_int() is limited to 32-bit
	return int(literal)

func as_bool() -> bool:
	if type != BOOLEAN:
		_print_type_error(BOOLEAN)

	return literal == "true"

func stringify_type(type: int) -> String:
	var constants: Dictionary = get_script().get_script_constant_map()
	return constants.keys()[constants.values().find(type)]

func _print_type_error(wrong_type: int) -> void:
	push_error("Use of token literal as '%s' for a token of type '%s'" % [
		stringify_type(wrong_type),
		stringify_type(type)
	])
