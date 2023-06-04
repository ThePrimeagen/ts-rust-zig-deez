package src;

// Typedef is something that i actually miss that languages like C# or Java.
// Actually very similar to use case of `type` keyword in TypeScript and very enjoyable to use.
typedef Token = {
	type:TokenType,
	literal:String
}

// Here you can see very neat Haxe's feature called `abstract`.
// In this particular case, this is abstract enum.
// The thing is, that abstract is actually don't exist after compilation.
// Right now it's TokenType enum, but at runtime it will be just string :O
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
