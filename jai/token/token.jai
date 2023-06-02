TokenType :: enum {
	Illegal;
	EOF;

	// Identifiers and literals
	Ident;
	Int;

	// Operators
	Assign; // =
	Plus; // +
	Minus; // -
	Bang; // !
	Asterisk; // *
	Slash; // /

	LT; // <
	GT; // >

	Eq; // ==
	Not_Eq; // !=
	
	// Delimiters
	Comma; // ,
	Semicolon; // ;

	LParen; // (
	RParen; // )
	LBrace; // {
	RBrace; // }

	// Keywords
	Function; // fn
	Let;
	True;
	False;
	If;
	Else;
	Return;
}

Token :: struct {
	type: TokenType;
	literal: string;
}

new_token :: (type: TokenType, literal: string) -> Token {
	return .{type, literal};
}
