#include <ostream>

#include "token.hpp"


std::string std::to_string(TokenType tokenType)
{
	switch (tokenType) {
		case TokenType::Illegal:        return "(illegal)";
		case TokenType::Eof:            return "(eof)";
		case TokenType::Identifier:     return "(identifier)";

		case TokenType::Integer:        return "(integer)";
		case TokenType::Comma:          return ",";
		case TokenType::Semicolon:      return ";";
		case TokenType::Lparen:         return "(";
		case TokenType::Rparen:         return ")";
		case TokenType::Lsquirly:       return "{";
		case TokenType::Rsquirly:       return "}";
		case TokenType::Lbracket:       return "[";
		case TokenType::Rbracket:       return "]";

		// OpTokenType::erator:         return "";
		case TokenType::Assign:         return "=";
		case TokenType::Plus:           return "+";
		case TokenType::Minus:          return "-";
		case TokenType::Bang:           return "!";
		case TokenType::Asterisk:       return "*";
		case TokenType::Slash:          return "/";
		case TokenType::Percent:        return "%";
		case TokenType::Tilde:          return "~";
		case TokenType::BitAnd:         return "&";
		case TokenType::BitOr:          return "|";
		case TokenType::BitEor:         return "^";
		case TokenType::And:            return "&&";
		case TokenType::Or:             return "||";

		case TokenType::Lt:             return "<";
		case TokenType::Gt:             return ">";
		case TokenType::Le:             return "<=";
		case TokenType::Ge:             return ">=";

		// KeTokenType::yword:          return "";
		case TokenType::Function:       return "fn";
		case TokenType::Let:            return "let";
		case TokenType::True:           return "true";
		case TokenType::False:          return "false";
		case TokenType::If:             return "if";
		case TokenType::Else:           return "else";
		case TokenType::Return:         return "return";

		case TokenType::Eq:             return "==";
		case TokenType::Not_eq:         return "!=";

		case TokenType::String:         return "(String)";
	}
	return std::string{"(unhandled "} + std::to_string((int) tokenType) + ")";
}


std::ostream& operator<<(std::ostream& os, const TokenType& tokenType)
{
	os << std::to_string(tokenType);
	return os;
}

std::ostream& operator<<(std::ostream& os, const Token& token)
{
	switch (token.type) {
		case TokenType::Identifier: os << token.literal; break;
		case TokenType::Integer:    os << token.literal; break;
		case TokenType::String:     os << "\"" << token.literal << "\""; break;
		default:                    os << token.type;    break;
	}
	return os;
}
