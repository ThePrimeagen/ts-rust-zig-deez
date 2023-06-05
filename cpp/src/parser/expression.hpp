#pragma once

#include <memory>
#include <vector>
#include <iosfwd>
#include <functional>

#include "token.hpp"
#include "value.hpp"
#include "environment.hpp"

class Lexer;

struct Expression;
using ExpressionP = std::unique_ptr<Expression>;

using StatementP = ExpressionP;

struct Expression
{
	virtual ~Expression() = default;

	static ExpressionP parse(Lexer& lexer);

	static ExpressionP parseOr(Lexer& lexer);
	static ExpressionP parseAnd(Lexer& lexer);
	static ExpressionP parseBitOr(Lexer& lexer);
	static ExpressionP parseBitXor(Lexer& lexer);
	static ExpressionP parseBitAnd(Lexer& lexer);

	static ExpressionP parseEquality(Lexer& lexer);
	static ExpressionP parseRelational(Lexer& lexer);
	static ExpressionP parseShift(Lexer& lexer);
	static ExpressionP parseSum(Lexer& lexer);
	static ExpressionP parseProduct(Lexer& lexer);
	static ExpressionP parseArrayIndex(Lexer& lexer);
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

struct AbstractFunctionExpression : public Expression, std::enable_shared_from_this<AbstractFunctionExpression>
{
	AbstractFunctionExpression(std::vector<std::string>&& parameters);
	virtual ~AbstractFunctionExpression() = default;

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;
	virtual Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const = 0;

	const auto& params() const noexcept { return parameters; }

protected:
	std::vector<std::string> parameters;
};

struct BuiltinFunctionExpression : public AbstractFunctionExpression
{
	BuiltinFunctionExpression(
		std::string&& name,
		std::vector<std::string>&& parameters,
		std::function<Value(const std::vector<Value>& arguments)>&& body)
	: AbstractFunctionExpression{std::move(parameters)}
	, name{std::move(name)}
	, body{std::move(body)}
	{}

	Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const override;

private:
	const std::string name;
	const std::function<Value(const std::vector<Value>& arguments)> body;
};

struct FunctionExpression : public AbstractFunctionExpression
{
	FunctionExpression(std::vector<std::string>&& parameters, StatementP&& body);

	static ExpressionP parse(Lexer& lexer);
	Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const override;

	const auto& params() const noexcept { return parameters; }

	std::shared_ptr<FunctionExpression> shared_from_this() { return std::static_pointer_cast<FunctionExpression>(AbstractFunctionExpression::shared_from_this()); }

private:
	EnvironmentP parentEnv;
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

struct StringLiteralExpression : public Expression
{
	StringLiteralExpression(const std::string value)
	: value{value} {}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const std::string value;
};

struct ArrayLiteralExpression : public Expression
{
	ArrayLiteralExpression(std::vector<ExpressionP>&& elements)
	: elements{std::move(elements)} {}
	static ExpressionP parse(Lexer& lexer);

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	std::vector<ExpressionP> elements;
};

struct ArrayIndexExpression : public Expression
{
	ArrayIndexExpression(ExpressionP&& array, ExpressionP&& index)
	: array{std::move(array)}
	, index{std::move(index)}
	{
	}

	void print(std::ostream& str) const override;
	Value eval(EnvironmentP env) const override;

private:
	const ExpressionP array;
	const ExpressionP index;
};



std::ostream& operator<<(std::ostream& os, const Expression& dt);
