module token

pub enum TokenType {
	illegal
	eof
	ident
	int
	string
	assign
	plus
	minus
	bang
	asterisk
	forward_slash
	less_than
	greater_than
	equal
	not_equal
	comma
	semicolon
	colon
	l_paren
	r_paren
	l_squirly
	r_squirly
	l_bracket
	r_bracket
	function
	let
	@true // @ escapes V keywords
	@false
	@if
	@else
	@return
}

pub struct Token {
pub:
	@type TokenType
	value string
}
