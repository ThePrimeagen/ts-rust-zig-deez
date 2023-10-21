#pragma once

#include "lexer/lexer.hh"
#include "ast/ast.hh"
#include <memory>
#include <charconv>
#include <functional>
#include <unordered_map>
#include <optional>

class Parser
{
	enum Precedence {
		LOWEST,
		LOGOR,
		LOGAND,
		BITOR,
		BITXOR,
		BITAND,
		EQUALS,
		LESSGREATER,
		SUM,
		PRODUCT,
		PREFIX,
		CALL,
		INDEX
	};

	Precedence precedenceFromToken(token_type tok);
	inline Precedence peekPrecedence() { return precedenceFromToken(peek_token.type); }
	inline Precedence currentPrecedence() { return precedenceFromToken(current_token.type); }

	token current_token;
	token peek_token;
	lexer &lex;
	std::vector<std::string> errors;

	void peekError(token_type t);
	void checkParserErrors();
	inline void onError(const std::string &msg) { errors.push_back(msg); }

	inline void advance_tokens()
	{
		current_token = std::move(peek_token);
		lex.next_token(peek_token);
	}
	inline bool curTokenIs(token_type t) { return current_token.type == t; }
	inline bool peekTokenIs(token_type t) { return peek_token.type == t; }
	inline bool expectPeek(token_type t) {
		if (!peekTokenIs(t)) {
			peekError(t);
			return false;
		}
		advance_tokens();
		return true;
	}

	std::unique_ptr<Identifier> parseIdentifier();
	std::unique_ptr<LetStatement> parseLet();
	std::unique_ptr<ReturnStatement> parseReturn();
	std::unique_ptr<IntegerLiteral> parseIntegerLiteral();
	std::unique_ptr<IntegerLiteral> parseHexIntegerLiteral();
	std::unique_ptr<FloatingPointLiteral> parseFloatingPointLiteral();
	std::unique_ptr<PrefixExpression> parsePrefixExpression();
	std::unique_ptr<InfixExpression> parseInfixExpression(expr_ptr &&left);
	std::unique_ptr<Expression> parseExpression(Precedence precedence);
	std::unique_ptr<ExpressionStatement> parseExpressionStatement();
	std::unique_ptr<Statement> parseStatement();
	std::unique_ptr<Expression> parseGroupedExpression();
	std::unique_ptr<BlockStatement> parseBlockStatement();
	std::unique_ptr<IfExpression> parseIfExpression();
	std::shared_ptr<std::vector<std::string>> parseFunctionParameters();
	std::unique_ptr<FunctionLiteral> parseFunctionLiteral();
	std::optional<std::vector<expr_ptr>> parseExpressionList(token_type end_tok);
	std::unique_ptr<CallExpression> parseCallExpression(expr_ptr &&left);
	std::unique_ptr<ArrayLiteral> parseArrayLiteral();
	std::unique_ptr<IndexExpression> parseIndexExpression(expr_ptr &&left);
	std::unique_ptr<HashLiteral> parseHashLiteral();

	using prefixParseFn = std::function<expr_ptr()>;
	using infixParseFn = std::function<expr_ptr(expr_ptr &&)>;

	std::unordered_map<token_type, prefixParseFn> prefixParseFns{
		{token_type::Identifier, [this]() { return parseIdentifier(); }},
		{token_type::Integer, [this]() { return parseIntegerLiteral(); }},
		{token_type::HexInteger, [this]() { return parseHexIntegerLiteral(); }},
		{token_type::FloatingPoint, [this]() { return parseFloatingPointLiteral(); }},
		{token_type::Dash, [this]() { return parsePrefixExpression(); }},
		{token_type::Bang, [this]() { return parsePrefixExpression(); }},
		{token_type::Tilde, [this]() { return parsePrefixExpression(); }},
		{token_type::True, [this]() { return std::make_unique<BooleanLiteral>(true); }},
		{token_type::False, [this]() {return std::make_unique<BooleanLiteral>(false); }},
		{token_type::String, [this]() {return std::make_unique<StringLiteral>(current_token.literal); }},
		{token_type::LParen, [this]() {return parseGroupedExpression(); }},
		{token_type::If, [this]() { return parseIfExpression(); }},
		{token_type::Function, [this]() { return parseFunctionLiteral(); }},
		{token_type::LBracket, [this]() { return parseArrayLiteral(); }},
		{token_type::LSquirly, [this]() { return parseHashLiteral(); }}
	};

	std::unordered_map<token_type,infixParseFn> infixParseFns{
		{token_type::Plus, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Dash, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Asterisk, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::ForwardSlash, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::LessThan, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::GreaterThan, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Equal, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::NotEqual, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Ampersand, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Pipe, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::Hat, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::LogicAnd, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::LogicOr, [this](expr_ptr &&left) { return parseInfixExpression(std::move(left)); }},
		{token_type::LParen, [this](expr_ptr &&left) { return parseCallExpression(std::move(left)); }},
		{token_type::LBracket, [this](expr_ptr &&left) { return parseIndexExpression(std::move(left)); }}
	};

public:
	explicit Parser(lexer &l);
	std::shared_ptr<Program> parse();

	inline bool hasErrors() {
		return !errors.empty();
	}

	void printErrors();
};