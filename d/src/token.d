module token;
import std.conv;
import std.typecons;
import std.traits;

struct Token {
    TokenType type;
    string literal;

    this(TokenType type, string literal) {
        this.type = type;
        this.literal = literal;
    }
    this(TokenType type, char literal) {
        this.type = type;
        this.literal = literal.to!string;
    }
}

enum TokenType {
    ILLEGAL = "ILLEGAL",
    EOF = "EOF",
    
    // Identifiers + literals
    IDENT = "IDENT", // add, foobar, x, y, ...
    INT = "INT", // 1343456

    // Operators
    ASSIGN = "=",
    PLUS = "+",
	MINUS    = "-",
	BANG     = "!",
	ASTERISK = "*",
	SLASH = "/",

	LT = "<",
	GT = ">",

	EQ     = "==",
	NOT_EQ = "!=",
    
    COMMA = ",",
    SEMICOLON = ";",

    LPAREN = "(",
    RPAREN = ")",
    LBRACE = "{",
    RBRACE = "}",

    // Keywords
    FUNCTION = "FUNCTION",
    LET = "LET",
	TRUE = "TRUE",
	FALSE = "FALSE",
	IF = "IF",
	ELSE = "ELSE",
	RETURN = "RETURN",
}
private alias TT = TokenType;

enum TokenType[string] keywords = [
    "fn": TT.FUNCTION,
    "let": TT.LET,
	"true": TT.TRUE,
	"false": TT.FALSE,
	"if": TT.IF,
	"else": TT.ELSE,
	"return": TT.RETURN,
];


TokenType lookupIdent(string ident) {
    if (ident in keywords) {
        return keywords[ident];
    }
    return TT.IDENT;
}
