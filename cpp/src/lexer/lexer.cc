#include <cctype>
#include <string>
#include <stdexcept>
#include <iostream>
#include <algorithm>

#include "lexer.hh"

Lexer::Lexer(std::string_view input)
: input_{input}
, position_{input_.begin()}
{
	next();
}

inline bool isWhitespace(char ch)
{
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
}

inline bool isIdentifier(char ch)
{
	return isalnum(ch) || ch == '_';
}

bool Lexer::swallow(char ch) noexcept
{
	if (position_ >= input_.end() || *position_ != ch)
		return false;
	position_++;
	return true;
}

void Lexer::next() noexcept
{
	token_ = nextToken();
	//std::cout << token_.type << ", " << token_.literal << "\n"; 
}

bool Lexer::get(TokenType tokenType)
{
	if (type() != tokenType)
		return false;
	next();
	return true;
}

Token Lexer::fetch(TokenType tokenType)
{
	Token token = peek();
	if (token.type != tokenType)
		throw std::runtime_error("unexpected token '" + std::to_string(token.type) + "' is not '" + std::to_string(tokenType) + "'");
	next();
	return token;
}


Token Lexer::nextToken() noexcept
{
	// skip whitespace
	position_ = std::find_if_not(position_, input_.end(), isWhitespace);

	// check if we're already off the end
	if (position_ >= input_.end())
		return { TokenType::Eof };

	// peek the next character
	const auto start_position = position_++;
	const auto ch = *start_position;

	// test for single-character tokens
	switch (ch) {
		case ',': return { TokenType::Comma     };
		case ';': return { TokenType::Semicolon };
		case '(': return { TokenType::Lparen    };
		case ')': return { TokenType::Rparen    };
		case '{': return { TokenType::Lsquirly  };
		case '}': return { TokenType::Rsquirly  };
		case '[': return { TokenType::Lbracket  };
		case ']': return { TokenType::Rbracket  };

		case '+': return { TokenType::Plus      };
		case '-': return { TokenType::Minus     };
		case '*': return { TokenType::Asterisk  };
		case '/': return { TokenType::Slash     };
		case '%': return { TokenType::Percent   };

		case '~': return { TokenType::Tilde     };
		case '^': return { TokenType::BitEor    };
		case '&': return { swallow('&') ? TokenType::And : TokenType::BitAnd };
		case '|': return { swallow('|') ? TokenType::Or  : TokenType::BitOr };

		case '<': return { swallow('=') ? TokenType::Le  : TokenType::Lt };
		case '>': return { swallow('=') ? TokenType::Ge  : TokenType::Gt };
		case '=': return { swallow('=') ? TokenType::Eq  : TokenType::Assign };
		case '!': return { swallow('=') ? TokenType::Not_eq : TokenType::Bang };

		case '"':
		{
			std::string str;
			char strch;
			while (position_ < input_.end() && (strch = *position_++) != '"') {
				if (strch == '\\') {
					if (position_ == input_.end())
						throw std::runtime_error("unterminated string");

					auto escaped = *position_++;
					switch (escaped) {
						case 'a':  escaped = '\a'; break;
						case 'b':  escaped = '\b'; break;
						case 'f':  escaped = '\f'; break;
						case 'n':  escaped = '\n'; break;
						case 'r':  escaped = '\r'; break;
						case 't':  escaped = '\t'; break;
						case 'v':  escaped = '\v'; break;
						case '\\': escaped = '\\'; break;
						case '\'': escaped = '\''; break;
						case '\"': escaped = '\"'; break;
						default:
							throw std::runtime_error(std::string{"illegal escape: '\\"} + escaped + "'");
					}
					str += escaped;
				}
				else {
					str += strch;
				}
			}
			if (strch != '\"')
				throw std::runtime_error("unterminated string");
			auto iter = stringPool_.insert(stringPool_.end(), std::move(str));
			return { TokenType::String, *iter };
		}
	}

	// parse integer
	if (isdigit(ch)) {
		position_ = std::find_if_not(position_, input_.end(), isdigit);
		// TODO: ASSERT(position_ >= input_.end() || !isIdentifier(*position_));	// identifiers can't start with digits, eg. '123a'
		return { TokenType::Integer, { start_position, position_ } };
	}

	// parse word
	if (isIdentifier(ch)) {
		position_ = std::find_if_not(position_, input_.end(), isIdentifier);
		std::string_view literal{ start_position, position_ };

		// check for keywords
		if (literal == "fn")       return { TokenType::Function };
		if (literal == "let")      return { TokenType::Let      };
		if (literal == "true")     return { TokenType::True     };
		if (literal == "false")    return { TokenType::False    };
		if (literal == "if")       return { TokenType::If       };
		if (literal == "else")     return { TokenType::Else     };
		if (literal == "return")   return { TokenType::Return   };

		// identifier
		return { TokenType::Identifier, literal };
	}

	return { TokenType::Illegal, { start_position, start_position + 1} };
}
