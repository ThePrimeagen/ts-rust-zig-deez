#pragma once

#include <iosfwd>
#include <string_view>

enum class TokenType
{
	Illegal,
	Eof,

	Identifier,
	Integer,
	String,
	True,
	False,

	Dollar,
	Comma,
	Semicolon,
	Colon,

	Lparen,
	Rparen,
	Lsquirly,
	Rsquirly,
	Lbracket,
	Rbracket,

	// Operators
	Assign,
	Plus,
	Minus,

	Bang,
	Tilde,

	Asterisk,
	Slash,
	Percent,


	BitAnd,
	BitOr,
	BitEor,
	And,
	Or,

	Eq,
	Not_eq,
	Lt,
	Gt,
	Le,
	Ge,

	// Keywords
	Function,
	Let,
	If,
	Else,
	Return,
};

using Identifier = std::string;

struct Token final
{
	TokenType type;
	std::string_view literal = {};
};


std::ostream& operator<<(std::ostream& os, const TokenType& token);
std::ostream& operator<<(std::ostream& os, const Token& token);

namespace std
{
	std::string to_string(TokenType type);
	std::string to_string(const Token& type);
}
