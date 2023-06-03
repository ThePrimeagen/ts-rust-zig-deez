#include <charconv>
#include <sstream>

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
	switch(op) {
		case TokenType::Asterisk: return std::get<int64_t>(evaluatedLeft) *  std::get<int64_t>(evaluatedRight);
		case TokenType::Slash:    return std::get<int64_t>(evaluatedLeft) /  std::get<int64_t>(evaluatedRight);
		case TokenType::Plus:     return std::get<int64_t>(evaluatedLeft) +  std::get<int64_t>(evaluatedRight);
		case TokenType::Minus:    return std::get<int64_t>(evaluatedLeft) -  std::get<int64_t>(evaluatedRight);
		case TokenType::Lt:       return std::get<int64_t>(evaluatedLeft) <  std::get<int64_t>(evaluatedRight);
		case TokenType::Gt:       return std::get<int64_t>(evaluatedLeft) >  std::get<int64_t>(evaluatedRight);
		case TokenType::Le:       return std::get<int64_t>(evaluatedLeft) <= std::get<int64_t>(evaluatedRight);
		case TokenType::Ge:       return std::get<int64_t>(evaluatedLeft) >= std::get<int64_t>(evaluatedRight);
		case TokenType::Eq:       return evaluatedLeft == evaluatedRight;
		case TokenType::Not_eq:   return evaluatedLeft != evaluatedRight;
	}
	throw std::runtime_error("evaluatedLeft " + std::to_string(op));
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


// FunctionExpression

FunctionExpression::FunctionExpression(
	std::vector<Identifier>&& parameters,
	StatementP&& body
)
: parameters{std::move(parameters)}
, body{std::move(body)}
{
}

ExpressionP FunctionExpression::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Function);
	lexer.fetch(TokenType::Lparen);

	std::vector<Identifier> parameters;
	while (lexer.type() != TokenType::Rparen) {
		auto token = lexer.fetch(TokenType::Identifier);
		parameters.push_back(token.literal);
		lexer.get(TokenType::Comma);
	}

	lexer.fetch(TokenType::Rparen);

	return std::make_unique<FunctionExpression>(
		std::move(parameters),
		Statement::parseStatement(lexer)
	);
}

Value FunctionExpression::eval(EnvironmentP env) const
{
	return std::make_pair(this, env);
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

void FunctionExpression::print(std::ostream& os) const
{
	os << "fn(";
	auto first = true;
	for (const auto& parameter : parameters) {
		if (!first)
			os << ",";
		first = false;
		os << parameter;
	}
	os << ") " << *body << ";";
}


// IdentifierExpression

Value IdentifierExpression::eval(EnvironmentP env) const
{
	return env->get(identifier);
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

