#pragma once

#include <iosfwd>
#include <string_view>

enum class TokenType
{
	Illegal,
	Eof,
	Identifier,
	Integer,

	Comma,
	Semicolon,
	Lparen,
	Rparen,
	Lsquirly,
	Rsquirly,

	// Operators
	Assign,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,
	Lt,
	Gt,
	Le,
	Ge,

	// Keywords
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,

	Eq,
	Not_eq,

	String,
	Lbracket,
	Rbracket,
};

using Identifier = std::string_view;

struct Token final
{
	TokenType type;
	Identifier literal = {};
};


std::ostream& operator<<(std::ostream& os, const TokenType& token);
std::ostream& operator<<(std::ostream& os, const Token& token);

namespace std
{
	std::string to_string(TokenType type);
}
