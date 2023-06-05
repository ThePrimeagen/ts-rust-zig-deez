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

	Comma,
	Semicolon,
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
	Asterisk,
	Slash,
	Percent,

	Bang,
	Tilde,

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
	True,
	False,
	If,
	Else,
	Return,
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
