#include <charconv>
#include <sstream>
#include <iostream>

#include "lexer.hh"
#include "expression.hpp"
#include "statement.hpp"

// Expression

ExpressionP Expression::parse(Lexer& lexer)
{
	auto tokenType = lexer.type();
	switch (tokenType) {
		case TokenType::If:
			return IfStatement::parse(lexer);
	}

	return parseEquality(lexer);
}

ExpressionP Expression::parseEquality(Lexer& lexer)
{
	auto left = parseCompare(lexer);

	while (true) {
		auto tokenType = lexer.type();
		switch (tokenType) {
			case TokenType::Eq:
			case TokenType::Not_eq:
				lexer.next();
				left = std::make_unique<BinaryExpression>(
					tokenType,
					std::move(left),
					parseCompare(lexer)
				);
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseCompare(Lexer& lexer)
{
	auto left = parseSum(lexer);

	auto tokenType = lexer.type();
	switch (tokenType) {
		case TokenType::Lt:
		case TokenType::Gt:
		case TokenType::Le:
		case TokenType::Ge:
			lexer.next();
			return std::make_unique<BinaryExpression>(
				tokenType,
				std::move(left),
				parseSum(lexer)
			);
	}

	return left;
}

ExpressionP Expression::parseSum(Lexer& lexer)
{
	auto left = parseProduct(lexer);

	while(true) {
		auto tokenType = lexer.type();
		switch (tokenType) {
			case TokenType::Plus:
			case TokenType::Minus:
				lexer.next();
				left = std::make_unique<BinaryExpression>(
					tokenType,
					std::move(left),
					parseProduct(lexer)
				);
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseProduct(Lexer& lexer)
{
	auto left = parsePrefix(lexer);

	while(true) {
		auto tokenType = lexer.type();
		switch (tokenType) {
			case TokenType::Asterisk:
			case TokenType::Slash:
				lexer.next();
				left = std::make_unique<BinaryExpression>(
					tokenType,
					std::move(left),
					parsePrefix(lexer)
				);
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parsePrefix(Lexer& lexer)
{
	auto tokenType = lexer.type();
	switch (tokenType) {
		case TokenType::Minus:
		case TokenType::Bang:
			lexer.next();
			return std::make_unique<UnaryExpression>(
				tokenType,
				parsePrefix(lexer)
			);
	}

	return parseCall(lexer);
}

ExpressionP Expression::parseCall(Lexer& lexer)
{
	auto prefix = Expression::parseGrouped(lexer);
	if (!prefix)
		return {};

	if (!lexer.get(TokenType::Lparen))
		return prefix;

	std::vector<ExpressionP> args;
	while (lexer.type() != TokenType::Rparen) {
		args.push_back(Expression::parse(lexer));
		lexer.get(TokenType::Comma);
	}

	lexer.fetch(TokenType::Rparen);

	return std::make_unique<CallExpression>(
		std::move(prefix),
		std::move(args)
	);
}


ExpressionP Expression::parseGrouped(Lexer& lexer)
{
	const auto tokenType = lexer.type();
	if (tokenType == TokenType::Function) {
		return FunctionExpression::parse(lexer);
	}

	if (tokenType != TokenType::Lparen)
		return parseValue(lexer);

	lexer.next();
	auto group = Expression::parse(lexer);

	lexer.fetch(TokenType::Rparen);

	return group;
}

ExpressionP Expression::parseValue(Lexer& lexer)
{
	const auto& token = lexer.peek();
	const auto tokenType = token.type;
	switch (tokenType) {
		case TokenType::Identifier:
		{
			auto identifier = token.literal;
			lexer.next();
			return std::make_unique<IdentifierExpression>(identifier);
		}
		case TokenType::Integer:
		{
			uint32_t value;
			std::from_chars(token.literal.data(), token.literal.data() + token.literal.size(), value);
			lexer.next();
			return std::make_unique<IntegerLiteralExpression>(value);
		}
		case TokenType::String:
		{
			std::string value{token.literal.data(), token.literal.size()};
			lexer.next();
			return std::make_unique<StringLiteralExpression>(value);
		}
		case TokenType::True:
		case TokenType::False:
			lexer.next();
			return std::make_unique<BooleanLiteralExpression>(tokenType == TokenType::True);
	}
	throw std::runtime_error("unexpected value token: '" + std::to_string(tokenType) + "'");
}

std::ostream& operator<<(std::ostream& os, const Expression& expression)
{
	expression.print(os);
	return os;
}


// BinaryExpresison

Value BinaryExpression::eval(EnvironmentP env) const
{
	auto evaluatedLeft = left->eval(env);
	auto evaluatedRight = right->eval(env);

	return std::visit(overloaded{
			[this](int64_t left, int64_t right) -> Value {
				switch(op) {
					case TokenType::Asterisk: return left * right;
					case TokenType::Slash:    return left / right;
					case TokenType::Plus:     return left + right;
					case TokenType::Minus:    return left - right;
					case TokenType::Lt:       return left < right;
					case TokenType::Gt:       return left > right;
					case TokenType::Le:       return left <=right;
					case TokenType::Ge:       return left >=right;
					case TokenType::Eq:       return left == right;
					case TokenType::Not_eq:   return left != right;
				}
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
			[this](bool left, bool right) -> Value{
				switch(op) {
					case TokenType::Eq:       return left == right;
					case TokenType::Not_eq:   return left != right;
				}
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
			[this](const std::string& left, const std::string& right) -> Value {
				switch(op) {
					case TokenType::Plus:     return left + right;
					case TokenType::Lt:       return left < right;
					case TokenType::Gt:       return left > right;
					case TokenType::Le:       return left <=right;
					case TokenType::Ge:       return left >=right;
					case TokenType::Eq:       return left == right;
					case TokenType::Not_eq:   return left != right;
				}
				throw std::runtime_error("invalid infix operation " + left + " " + std::to_string(op) + " " + right);
			},
			[this](const auto& left, const auto& right) -> Value {
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
		},
		evaluatedLeft,
		evaluatedRight
	);
}

void BinaryExpression::print(std::ostream& os) const
{
	os << *left << op << *right;
}


// UnaryExpression

Value UnaryExpression::eval(EnvironmentP env) const
{
	auto evaluatedValue = value->eval(env);
	switch(op) {
		case TokenType::Minus:
			return -std::get<int64_t>(evaluatedValue);
		case TokenType::Bang:
			if (!std::holds_alternative<bool>(evaluatedValue))
				return false;
			return !std::get<bool>(evaluatedValue);
	}
	throw std::runtime_error("invalid unary operation: " + std::to_string(op));
}

void UnaryExpression::print(std::ostream& os) const
{
	os << op << *value;
}


// CallExpression

Value CallExpression::eval(EnvironmentP env) const
{
	auto [fn, closureEnv] = std::get<BoundFunction>(function->eval(env));
	return fn->call(closureEnv, env, arguments);
}

void CallExpression::print(std::ostream& os) const
{
	os << "call " << *function << "(";
	auto first = true;
	for (const auto& arg : arguments) {
		if (first)
			first = false;
		else
			os << ", ";
		os << *arg;
	}
	os << ")";
}

// AbstractFunctionExpression

AbstractFunctionExpression::AbstractFunctionExpression(std::vector<std::string>&& parameters)
: parameters{std::move(parameters)}
{	
}

Value AbstractFunctionExpression::eval(EnvironmentP env) const
{
	//auto f = shared_from_this();
	return std::make_pair(this, env);
}

void AbstractFunctionExpression::print(std::ostream& os) const
{
	os << "fn(";
	auto first = true;
	for (const auto& parameter : parameters) {
		if (!first)
			os << ",";
		first = false;
		os << parameter;
	}
	//os << ") " << *body << ";";
	os << ")";
}


// FunctionExpression

FunctionExpression::FunctionExpression(
	std::vector<std::string>&& parameters,
	StatementP&& body
)
: AbstractFunctionExpression{std::move(parameters)}
, body{std::move(body)}
{
}

ExpressionP FunctionExpression::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Function);
	lexer.fetch(TokenType::Lparen);

	std::vector<std::string> parameters;
	while (lexer.type() != TokenType::Rparen) {
		auto token = lexer.fetch(TokenType::Identifier);
		parameters.push_back(std::string{token.literal});
		lexer.get(TokenType::Comma);
	}

	lexer.fetch(TokenType::Rparen);

	return std::make_unique<FunctionExpression>(
		std::move(parameters),
		Statement::parseStatement(lexer)
	);
}

Value FunctionExpression::call(
	EnvironmentP closureEnv,
	EnvironmentP callerEnv,
	const std::vector<ExpressionP>& arguments
) const
{
	// TODO: copy-on-write
	auto locals = std::make_shared<Environment>(closureEnv);

	auto argumentIter = arguments.begin();
	for (const auto& parameterName : parameters) {

		Value argumentValue;
		if (argumentIter == arguments.end()) {
			argumentValue = nil;
		}
		else {
			const auto& argument= *(argumentIter++);
			argumentValue = argument->eval(callerEnv);
		}
		locals->set(parameterName, argumentValue);
	}

	try {
		return body->eval(locals);
	}
	catch (const Value& value) {
		return value;
	}
}


// LenFunctionExpression
Value LenFunctionExpression::call(
	EnvironmentP closureEnv,
	EnvironmentP callerEnv,
	const std::vector<ExpressionP>& arguments
) const
{
	if (arguments.size() != 1)
		throw std::runtime_error("wrong number of arguments to len(): " + std::to_string(arguments.size()));

	auto value = arguments[0]->eval(closureEnv);
	return std::visit(overloaded{
		[](const std::string& str) { return static_cast<int64_t>(str.length()); },
		[](const auto& value) -> int64_t {
			throw std::runtime_error("invalid argument to len(): " + std::to_string(value));
		}
	}, value);
}


// IdentifierExpression

LenFunctionExpression LenFunctionExpression::singleton;

Value IdentifierExpression::eval(EnvironmentP env) const
{
	auto value = env->get(identifier);
	if (value == nil) {
		if (identifier == "len") {
			return std::make_pair(&LenFunctionExpression::singleton, EnvironmentP{});
		}
	}
	return value;
}

void IdentifierExpression::print(std::ostream& os) const
{
	os << identifier;
}


//IntegerLiteralExpression

Value IntegerLiteralExpression::eval(EnvironmentP env) const
{
	return value;
}

void IntegerLiteralExpression::print(std::ostream& os) const
{
	os << value;
}


// BooleanLiteralExpression

Value BooleanLiteralExpression::eval(EnvironmentP env) const
{
	return value;
}

void BooleanLiteralExpression::print(std::ostream& os) const
{
	os << value;
}


// StringLiteralExpression

Value StringLiteralExpression::eval(EnvironmentP env) const
{
	return value;
}

void StringLiteralExpression::print(std::ostream& os) const
{
	os << value;
}

