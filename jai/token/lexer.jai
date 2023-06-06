
Lexer :: struct {
	input: string;
	// Current position in input (points to current char)
	position: int;
	// Current reading position in input (after current char)
	read_position: int;
	// Current char under examination
	ch: string;
}

new_lexer :: (input: string) -> Lexer {
	return .{
		input = input,
	};
}

read_char :: (lex: *Lexer) {
	if lex.read_position >= lex.input.count {
		lex.ch = "\0";
	} else {
		lex.ch = slice(lex.input, lex.read_position, 1);
	}
	lex.position = lex.read_position;
	lex.read_position += 1;
}

peek_char :: (lex: *Lexer) -> string {
	if lex.read_position >= lex.input.count then return "\0";
	else return slice(lex.input, lex.read_position, 1);
}

next_token :: (lex: *Lexer) -> Token {
	tok: Token;

	skip_whitespace(lex);
	
	if lex.ch == {
		case "=";
		if peek_char(lex) == {
			case "=";
			read_char(lex);
			tok = new_token(.Eq, slice(
				lex.input,
				lex.read_position-2,
				2));

			case;
			tok = new_token(.Assign, lex.ch);
		}

		case "+";
 		tok = new_token(.Plus, lex.ch);
		
		case "-";
 		tok = new_token(.Minus, lex.ch);

		case "!";
		if peek_char(lex) == {
			case "=";
			read_char(lex);
			tok = new_token(.Not_Eq, slice(
				lex.input,
				lex.read_position-2,
				2));
			
			case;
 			tok = new_token(.Bang, lex.ch);
		}
			
		case "/";
 		tok = new_token(.Slash, lex.ch);

		case "*";
 		tok = new_token(.Asterisk, lex.ch);

		case "<";
 		tok = new_token(.LT, lex.ch);

		case ">";
 		tok = new_token(.GT, lex.ch);

		case ";";
		tok = new_token(.Semicolon, lex.ch);

		case ",";
		tok = new_token(.Comma, lex.ch);
		
		case "(";
		tok = new_token(.LParen, lex.ch);

		case ")";
		tok = new_token(.RParen, lex.ch);

		case "{";
		tok = new_token(.LBrace, lex.ch);

		case "}";
		tok = new_token(.RBrace, lex.ch);

		case "\0";
		tok = new_token(.EOF, lex.ch);

		case;
		if is_letter(lex.ch[0]) {
			tok.literal = read_ident(lex);
			tok.type = lookup_ident(tok.literal);
			return tok; // because we do read_char() in read_ident()
		}
		if is_digit(lex.ch[0]) {
			tok.literal = read_number(lex);
			tok.type = .Int;
			return tok; // because we do read_char() in read_number()
		}
		tok = new_token(.Illegal, lex.ch);
	}

	read_char(lex);
	return tok;
}

#scope_file

is_letter :: (ch: u8) -> bool {
	return (ch >= #char "A" && ch <= #char "Z") || (ch >= #char "a" && ch <= #char "z");
}

is_digit :: (ch: u8) -> bool {
	return ch >= #char "0" && ch <= #char "9";
}

lookup_ident :: (ident: string) -> TokenType {
	tt: TokenType;
	if ident == {
		case "let";    tt = .Let;
		case "fn";     tt = .Function;
		case "true";   tt = .True;
		case "false";  tt = .False;
		case "if";     tt = .If;
		case "else";   tt = .Else;
		case "return"; tt = .Return;
		case; tt = .Ident;
	}
	return tt;
}

skip_whitespace :: (lex: *Lexer) {
	while lex.ch == " " || lex.ch == "\t" || lex.ch == "\n" || lex.ch == "\r" {
		read_char(lex);
	}
}

// @TODO figure out a way to combine read_number and read_ident into one function that takes a function like is_digit or is_letter as an argument.
read_number :: (lex: *Lexer) -> string {
	position := lex.position;
	while is_digit(lex.ch[0]) read_char(lex);
	return slice(lex.input, position, lex.position - position);
}

read_ident :: (lex: *Lexer) -> string {
	position := lex.position;
	while is_letter(lex.ch[0]) read_char(lex);
	return slice(lex.input, position, lex.position - position);
}
