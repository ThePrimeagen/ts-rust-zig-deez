#pragma once

#include <string_view>

#include "token.hpp"

class Lexer final
{
	public:
		Lexer(std::string_view);
		void next() noexcept;
		Token fetch(TokenType type);
		bool get(TokenType type);

		const Token& peek() const noexcept { return token_; }
		bool peekIs(TokenType type) const noexcept { return peek().type == type; }
		bool eof() const noexcept { return peekIs(TokenType::Eof); }

	private:
		Token nextToken() noexcept;
		bool swallow(char ch) noexcept;

		Token token_;

		const std::string_view input_;
		std::string_view::iterator position_;
};
