#include <ostream>
#include <sstream>

#include "token.hpp"


std::ostream& operator<<(std::ostream& os, const TokenType& tokenType)
{
	switch (tokenType) {
		case TokenType::Illegal:        os << "(illegal)"; break;
		case TokenType::Eof:            os << "(eof)"; break;
		case TokenType::Identifier:     os << "(identifier)"; break;

		case TokenType::Integer:        os << "(integer)"; break;
		case TokenType::Comma:          os << ","; break;
		case TokenType::Semicolon:      os << ";"; break;
		case TokenType::Dollar:         os << "$"; break;
		case TokenType::Colon:          os << ":"; break;
		
		case TokenType::Lparen:         os << "("; break;
		case TokenType::Rparen:         os << ")"; break;
		case TokenType::Lsquirly:       os << "{"; break;
		case TokenType::Rsquirly:       os << "}"; break;
		case TokenType::Lbracket:       os << "["; break;
		case TokenType::Rbracket:       os << "]"; break;

		// OpTokenType::erator:         os << ""; break;
		case TokenType::Assign:         os << "="; break;
		case TokenType::Plus:           os << "+"; break;
		case TokenType::Minus:          os << "-"; break;
		case TokenType::Bang:           os << "!"; break;
		case TokenType::Asterisk:       os << "*"; break;
		case TokenType::Slash:          os << "/"; break;
		case TokenType::Percent:        os << "%"; break;
		case TokenType::Tilde:          os << "~"; break;
		case TokenType::BitAnd:         os << "&"; break;
		case TokenType::BitOr:          os << "|"; break;
		case TokenType::BitEor:         os << "^"; break;
		case TokenType::And:            os << "&&"; break;
		case TokenType::Or:             os << "||"; break;

		case TokenType::Lt:             os << "<"; break;
		case TokenType::Gt:             os << ">"; break;
		case TokenType::Le:             os << "<="; break;
		case TokenType::Ge:             os << ">="; break;

		// KeTokenType::yword:          os << ""; break;
		case TokenType::Function:       os << "fn"; break;
		case TokenType::Let:            os << "let"; break;
		case TokenType::True:           os << "true"; break;
		case TokenType::False:          os << "false"; break;
		case TokenType::If:             os << "if"; break;
		case TokenType::Else:           os << "else"; break;
		case TokenType::Return:         os << "return"; break;

		case TokenType::Eq:             os << "=="; break;
		case TokenType::Not_eq:         os << "!="; break;

		case TokenType::String:         os << "(String)"; break;
		default:
			os << "(unhandled " << static_cast<int>(tokenType) << ")";	
			break;
	}
	return os;
}

std::ostream& operator<<(std::ostream& os, const Token& token)
{
	switch (token.type) {
		case TokenType::Identifier: os << token.literal; break;
		case TokenType::Integer:    os << token.literal; break;
		case TokenType::String:     os << "\"" << token.literal << "\""; break;
		default:                    os << token.type; break;
	}
	return os;
}


std::string std::to_string(TokenType tokenType)
{
	std::stringstream ss;
	ss << tokenType;
	return ss.str();
}

std::string std::to_string(const Token& token)
{
	std::stringstream ss;
	ss << token;
	return ss.str();
}
