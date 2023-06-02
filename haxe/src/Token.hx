package src;

typedef Token = {
	type:TokenType,
	literal:String
}

enum abstract TokenType(String) {
	var Illegal = "ILLEGAL";
	var Eof = "EOF";
	var Ident = "IDENT";
	var If = "if";
	var Return = "return";
	var True = "true";
	var False = "false";
	var Else = "else";
	var Int = "INT";
	var Assign = "=";
	var NotEqual = "!=";
	var Equal = "==";
	var Plus = "+";
	var Comma = ";";
	var Semicolon = ";";
	var LParen = "(";
	var RParen = ")";
	var LSquirly = "{";
	var RSquirly = "}";
	var Function = "FUNCTION";
	var Let = "LET";
	var Bang = "!";
	var Dash = "-";
	var ForwardSlash = "/";
	var Asterisk = "*";
	var LessThan = "<";
	var GreaterThan = ">";
}
