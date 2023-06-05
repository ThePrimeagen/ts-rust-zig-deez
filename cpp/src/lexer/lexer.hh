#pragma once

#include <string_view>
#include <vector>

#include "token.hpp"

class Lexer final
{
	public:
		Lexer(std::string_view);
		void next() noexcept;
		Token fetch(TokenType type);
		bool get(TokenType type);

		auto type() const noexcept { return token_.type; }
		auto literal() const noexcept { return token_.literal; }

		const Token& peek() const noexcept { return token_; }
		bool peekIs(TokenType tokenType) const noexcept { return type() == tokenType; }
		bool eof() const noexcept { return peekIs(TokenType::Eof); }

	private:
		Token nextToken() noexcept;
		bool swallow(char ch) noexcept;

		Token token_;

		const std::string_view input_;
		std::string_view::iterator position_;
		std::vector<std::string> stringPool_;
};
