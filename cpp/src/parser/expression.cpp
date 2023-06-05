#include <charconv>
#include <sstream>
#include <iostream>

#include "lexer.hh"
#include "expression.hpp"
#include "statement.hpp"
#include "builtins.hpp"

// Expression

ExpressionP Expression::parse(Lexer& lexer)
{
	auto tokenType = lexer.type();
	switch (tokenType) {
		case TokenType::If:
			return IfStatement::parse(lexer);
	}

	return parseOr(lexer);
}

ExpressionP Expression::parseOr(Lexer& lexer)
{
	auto left = parseAnd(lexer);
	while (lexer.peekIs(TokenType::Or)) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(TokenType::Or, std::move(left), parseAnd(lexer));
	}
	return left;
}

ExpressionP Expression::parseAnd(Lexer& lexer)
{
	auto left = parseBitOr(lexer);
	while (lexer.peekIs(TokenType::And)) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(TokenType::And, std::move(left), parseBitOr(lexer));
	}
	return left;
}

ExpressionP Expression::parseBitOr(Lexer& lexer)
{
	auto left = parseBitXor(lexer);
	while (lexer.peekIs(TokenType::BitOr)) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(TokenType::BitOr, std::move(left), parseBitXor(lexer));
	}
	return left;
}

ExpressionP Expression::parseBitXor(Lexer& lexer)
{
	auto left = parseBitAnd(lexer);
	while (lexer.peekIs(TokenType::BitEor)) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(TokenType::BitEor, std::move(left), parseBitAnd(lexer));
	}
	return left;
}

ExpressionP Expression::parseBitAnd(Lexer& lexer)
{
	auto left = parseEquality(lexer);
	while (lexer.peekIs(TokenType::BitAnd)) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(TokenType::BitAnd, std::move(left), parseEquality(lexer));
	}
	return left;
}

ExpressionP Expression::parseEquality(Lexer& lexer)
{
	auto left = parseRelational(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Eq || tokenType == TokenType::Not_eq; tokenType = lexer.type()) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(tokenType, std::move(left), parseRelational(lexer) );
	}
	return left;
}

ExpressionP Expression::parseRelational(Lexer& lexer)
{
	auto left = parseSum(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Lt || tokenType == TokenType::Gt || tokenType == TokenType::Le || tokenType == TokenType::Ge; tokenType = lexer.type()) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(tokenType, std::move(left), parseSum(lexer) );
	}
	return left;
}

ExpressionP Expression::parseSum(Lexer& lexer)
{
	auto left = parseProduct(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Plus || tokenType == TokenType::Minus; tokenType = lexer.type()) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(tokenType, std::move(left), parseProduct(lexer) );
	}
	return left;
}

ExpressionP Expression::parseProduct(Lexer& lexer)
{
	auto left = parseArrayIndex(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Asterisk || tokenType == TokenType::Slash || tokenType == TokenType::Percent; tokenType = lexer.type()) {
		lexer.next();
		left = std::make_unique<BinaryExpression>(tokenType, std::move(left), parseArrayIndex(lexer) );
	}
	return left;
}

ExpressionP Expression::parseArrayIndex(Lexer& lexer)
{
	auto left = parsePrefix(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Lbracket; tokenType = lexer.type()) {
		lexer.next();
		auto right = Expression::parse(lexer);
		lexer.fetch(TokenType::Rbracket);

		left = std::make_unique<ArrayIndexExpression>(
			std::move(left),
			std::move(right)
		);
	}
	return left;
}

ExpressionP Expression::parsePrefix(Lexer& lexer)
{
	auto tokenType = lexer.type();
	switch (tokenType) {
		case TokenType::Lbracket:
			return ArrayLiteralExpression::parse(lexer);
		case TokenType::Minus:
		case TokenType::Bang:
		case TokenType::Tilde:
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
	if (!prefix || !lexer.get(TokenType::Lparen))
		return prefix;

	std::vector<ExpressionP> args;
	while (!lexer.get(TokenType::Rparen)) {
		args.push_back(Expression::parse(lexer));
		lexer.get(TokenType::Comma);
	}

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
					case TokenType::Asterisk: return Value{left * right};
					case TokenType::Slash:    return Value{left / right};
					case TokenType::Percent:  return Value{left % right};
					case TokenType::Plus:     return Value{left + right};
					case TokenType::Minus:    return Value{left - right};
					case TokenType::BitAnd:   return Value{left & right};
					case TokenType::BitOr:    return Value{left | right};
					case TokenType::BitEor:   return Value{left ^ right};
					case TokenType::Lt:       return Value{left < right};
					case TokenType::Gt:       return Value{left > right};
					case TokenType::Le:       return Value{left <=right};
					case TokenType::Ge:       return Value{left >=right};
					case TokenType::Eq:       return Value{left == right};
					case TokenType::Not_eq:   return Value{left != right};
				}
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
			[this](bool left, bool right) -> Value{
				switch(op) {
					case TokenType::And:      return Value{left && right};
					case TokenType::Or:       return Value{left || right};
					case TokenType::Eq:       return Value{left == right};
					case TokenType::Not_eq:   return Value{left != right};
				}
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
			[this](const String& left, const String& right) -> Value {
				switch(op) {
					case TokenType::Plus:     return {left + right};
					case TokenType::Lt:       return {left < right};
					case TokenType::Gt:       return {left > right};
					case TokenType::Le:       return {left <=right};
					case TokenType::Ge:       return {left >=right};
					case TokenType::Eq:       return {left == right};
					case TokenType::Not_eq:   return {left != right};
				}
				throw std::runtime_error("invalid infix operation " + left + " " + std::to_string(op) + " " + right);
			},
			[this](const auto& left, const auto& right) -> Value {
				throw std::runtime_error("invalid infix operation " + std::to_string(left) + " " + std::to_string(op) + " " + std::to_string(right));
			},
		},
		evaluatedLeft.data,
		evaluatedRight.data
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
			return Value{-std::get<int64_t>(evaluatedValue.data)};
		case TokenType::Bang:
			if (!std::holds_alternative<bool>(evaluatedValue.data))
				return Value{false};
			return Value{!std::get<bool>(evaluatedValue.data)};
		case TokenType::Tilde:
			return Value{~std::get<int64_t>(evaluatedValue.data)};
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
	auto [fn, closureEnv] = std::get<BoundFunction>(function->eval(env).data);
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
	return Value{std::make_pair(this, env)};
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
	while (!lexer.get(TokenType::Rparen)) {
		auto token = lexer.fetch(TokenType::Identifier);
		parameters.push_back(std::string{token.literal});
		lexer.get(TokenType::Comma);
	}

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
		if (argumentIter != arguments.end()) {
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

// BuiltinFunctionExpression

Value BuiltinFunctionExpression::call(
	EnvironmentP closureEnv,
	EnvironmentP callerEnv,
	const std::vector<ExpressionP>& arguments
) const
{
	std::vector<Value> argumentValues;
	std::transform(
		arguments.cbegin(), arguments.cend(),
		std::back_inserter(argumentValues), [&callerEnv](const ExpressionP& element) { return element->eval(callerEnv); }
	);
	return body(std::move(argumentValues));
}

// IdentifierExpression

Value IdentifierExpression::eval(EnvironmentP env) const
{
	auto value = env->get(identifier);
	if (value == nil) {
		if (const auto iter = builtins.find(identifier); iter != builtins.end()) {
			return Value{std::make_pair(&(iter->second), EnvironmentP{})};
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
	return Value{value};
}

void IntegerLiteralExpression::print(std::ostream& os) const
{
	os << value;
}


// BooleanLiteralExpression

Value BooleanLiteralExpression::eval(EnvironmentP env) const
{
	return Value{value};
}

void BooleanLiteralExpression::print(std::ostream& os) const
{
	os << value;
}


// StringLiteralExpression

Value StringLiteralExpression::eval(EnvironmentP env) const
{
	return Value{value};
}

void StringLiteralExpression::print(std::ostream& os) const
{
	os << value;
}


// ArrayLiteralExpression

ExpressionP ArrayLiteralExpression::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Lbracket);

	std::vector<ExpressionP> elements;
	while (!lexer.get(TokenType::Rbracket)) {
		elements.push_back(Expression::parse(lexer));
		lexer.get(TokenType::Comma);
	}

	return std::make_unique<ArrayLiteralExpression>(std::move(elements));
}

Value ArrayLiteralExpression::eval(EnvironmentP env) const
{
	std::vector<Value> value;
	std::transform(
		elements.cbegin(), elements.cend(),
		std::back_inserter(value), [&env](const ExpressionP& element) { return element->eval(env); }
	);
	return Value{value};
}

void ArrayLiteralExpression::print(std::ostream& os) const
{
	os << "[";
	auto first = true;
	for (const auto& element : elements) {
		if (first)
			first = false;
		else
			os << ",";
		os << *element;
	}
	os << "]";
}


// ArrayIndexExpression

Value ArrayIndexExpression::eval(EnvironmentP env) const
{
	const auto& evluatedArray = array->eval(env).data;
	const auto& evaluatedIndex = index->eval(env).data;

	const auto& arrayValue = std::get<std::vector<Value>>(evluatedArray);
	const auto indexValue = std::get<int64_t>(evaluatedIndex);

	if (indexValue < 0 || indexValue >= static_cast<int64_t>(arrayValue.size()))
		return nil;

	return arrayValue[indexValue];
}

void ArrayIndexExpression::print(std::ostream& os) const
{
	os << array << "[" << index << "]";
}

