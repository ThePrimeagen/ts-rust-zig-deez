#pragma once

#include <memory>
#include <vector>
#include <iosfwd>

#include "token.hpp"
#include "value.hpp"
#include "environment.hpp"

class Lexer;

struct Expression;
using ExpressionP = std::unique_ptr<Expression>;

using StatementP = ExpressionP;

struct Expression
{
	static ExpressionP parse(Lexer& lexer);

	static ExpressionP parseEquality(Lexer& lexer);
	static ExpressionP parseCompare(Lexer& lexer);
	static ExpressionP parseSum(Lexer& lexer);
	static ExpressionP parseProduct(Lexer& lexer);
	static ExpressionP parsePrefix(Lexer& lexer);
	static ExpressionP parseCall(Lexer& lexer);
	static ExpressionP parseUnary(Lexer& lexer);
	static ExpressionP parseGrouped(Lexer& lexer);
	static ExpressionP parseValue(Lexer& lexer);

	static ExpressionP parseStatement(Lexer& lexer);

	virtual void print(std::ostream& str) const = 0;
	virtual Value eval(EnvironmentP env) const = 0;
};

struct BinaryExpression : public Expression
{
	BinaryExpression(TokenType op, ExpressionP&& left, ExpressionP&& right)
	: op{op}, left{std::move(left)}, right{std::move(right)} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	TokenType op;
	ExpressionP left;
	ExpressionP right;
};

struct UnaryExpression : public Expression
{
	UnaryExpression(TokenType op, ExpressionP&& value)
	: op{op}, value{std::move(value)} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const TokenType op;
	const ExpressionP value;
};

struct CallExpression : public Expression
{
	CallExpression(ExpressionP&& function, std::vector<ExpressionP>&& arguments)
	: function{std::move(function)}, arguments{std::move(arguments)} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const ExpressionP function;
	const std::vector<ExpressionP> arguments;
};

struct FunctionExpression : public Expression
{
	FunctionExpression(std::vector<Identifier>&& parameters, StatementP&& body);

	static ExpressionP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;
	Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const;

private:
	EnvironmentP parentEnv;
	std::vector<Identifier> parameters;
	StatementP body;
};


struct IdentifierExpression : public Expression
{
	IdentifierExpression(Identifier identifier)
	: identifier{identifier} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	Identifier identifier;
};

struct IntegerLiteralExpression : public Expression
{
	IntegerLiteralExpression(const int64_t value)
	: value{value} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const int64_t value;
};

struct BooleanLiteralExpression : public Expression
{
	BooleanLiteralExpression(const bool value)
	: value{value} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const bool value;
};



std::ostream& operator<<(std::ostream& os, const Expression& dt);
