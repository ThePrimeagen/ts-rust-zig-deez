#pragma once

#include <vector>

#include "expression.hpp"
#include "utils.hpp"


struct BuiltinFunctionExpression : public AbstractFunctionExpression
{
	BuiltinFunctionExpression(
		std::string&& name,
		std::vector<std::string>&& parameters,
		std::function<Value(const Array& arguments)>&& body)
	: AbstractFunctionExpression{std::move(parameters)}
	, name{std::move(name)}
	, body{std::move(body)}
	{}

	Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const override;

	const std::string name;

	static std::vector<BuiltinFunctionExpression> builtins;

private:
	const std::function<Value(const Array& arguments)> body;
};

struct BuiltinBinaryFunctionExpression : public AbstractFunctionExpression
{
	BuiltinBinaryFunctionExpression(
		std::string&& name,
		std::function<Value(const Value& left, const Value& right)>&& body
	)
	: AbstractFunctionExpression{{"x", "y"}}
	, name{std::move(name)}
	, body{std::move(body)}
	{}

	Value call(
		EnvironmentP closureEnv,
		EnvironmentP callerEnv,
		const std::vector<ExpressionP>& arguments
	) const override;

	Value call(const Value& left, const Value& right) const { return body(left, right); }

	const std::string name;

	template<typename T1, typename T2>
	static void error(std::string_view name, const T1& left, const T2& right) {
		throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::string{name} + " " + std::to_string(right));
	}

	static BuiltinBinaryFunctionExpression asterisk;
	static BuiltinBinaryFunctionExpression slash;
	static BuiltinBinaryFunctionExpression percent;
	static BuiltinBinaryFunctionExpression plus;
	static BuiltinBinaryFunctionExpression minus;
	static BuiltinBinaryFunctionExpression bitAnd;
	static BuiltinBinaryFunctionExpression bitOr;
	static BuiltinBinaryFunctionExpression bitEor;
	static BuiltinBinaryFunctionExpression lt;
	static BuiltinBinaryFunctionExpression gt;
	static BuiltinBinaryFunctionExpression le;
	static BuiltinBinaryFunctionExpression ge;
	static BuiltinBinaryFunctionExpression eq;
	static BuiltinBinaryFunctionExpression neq;
	static BuiltinBinaryFunctionExpression and_;	// ugh, 'and', 'or' conflict with some stupid macros
	static BuiltinBinaryFunctionExpression or_;

	static std::unordered_map<TokenType, BuiltinBinaryFunctionExpression*> builtins;

private:
	const std::function<Value(const Value& left, const Value& right)> body;
};
