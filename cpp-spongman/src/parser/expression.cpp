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
	while (lexer.get(TokenType::Or))
		left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::or_, std::move(left), parseAnd(lexer));
	return left;
}

ExpressionP Expression::parseAnd(Lexer& lexer)
{
	auto left = parseBitOr(lexer);
	while (lexer.get(TokenType::And))
		left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::and_, std::move(left), parseBitOr(lexer));
	return left;
}

ExpressionP Expression::parseBitOr(Lexer& lexer)
{
	auto left = parseBitXor(lexer);
	while (lexer.get(TokenType::BitOr))
		left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::bitOr, std::move(left), parseBitXor(lexer));
	return left;
}

ExpressionP Expression::parseBitXor(Lexer& lexer)
{
	auto left = parseBitAnd(lexer);
	while (lexer.get(TokenType::BitEor))
		left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::bitEor, std::move(left), parseBitAnd(lexer));
	return left;
}

ExpressionP Expression::parseBitAnd(Lexer& lexer)
{
	auto left = parseEquality(lexer);
	while (lexer.get(TokenType::BitAnd))
		left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::bitAnd, std::move(left), parseEquality(lexer));
	return left;
}

ExpressionP Expression::parseEquality(Lexer& lexer)
{
	auto left = parseRelational(lexer);
	for (auto tokenType = lexer.type(); ; tokenType = lexer.type()) {
		switch (tokenType) {
			case TokenType::Eq:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::eq, std::move(left), parseRelational(lexer));
				break;
			case TokenType::Not_eq:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::neq, std::move(left), parseRelational(lexer));
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseRelational(Lexer& lexer)
{
	auto left = parseSum(lexer);
	for (auto tokenType = lexer.type(); ; tokenType = lexer.type()) {
		switch (tokenType) {
			case TokenType::Lt:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::lt, std::move(left), parseSum(lexer));
				break;
			case TokenType::Gt:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::gt, std::move(left), parseSum(lexer));
				break;
			case TokenType::Le:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::le, std::move(left), parseSum(lexer));
				break;
			case TokenType::Ge:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::ge, std::move(left), parseSum(lexer));
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseSum(Lexer& lexer)
{
	auto left = parseProduct(lexer);
	for (auto tokenType = lexer.type(); ; tokenType = lexer.type()) {
		switch (tokenType) {
			case TokenType::Plus:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::plus, std::move(left), parseProduct(lexer));
				break;
			case TokenType::Minus:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::minus, std::move(left), parseProduct(lexer));
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseProduct(Lexer& lexer)
{
	auto left = parseArrayIndex(lexer);
	for (auto tokenType = lexer.type(); ; tokenType = lexer.type()) {
		switch (tokenType) {
			case TokenType::Asterisk:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::asterisk, std::move(left), parseArrayIndex(lexer));
				break;
			case TokenType::Slash:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::slash, std::move(left), parseArrayIndex(lexer));
				break;
			case TokenType::Percent:
				lexer.next();
				left = std::make_unique<BinaryExpression>(BuiltinBinaryFunctionExpression::percent, std::move(left), parseArrayIndex(lexer));
				break;
			default:
				return left;
		}
	}
}

ExpressionP Expression::parseArrayIndex(Lexer& lexer)
{
	auto left = parsePrefix(lexer);
	for (auto tokenType = lexer.type(); tokenType == TokenType::Lbracket; tokenType = lexer.type()) {
		lexer.next();
		auto right = Expression::parse(lexer);
		lexer.fetch(TokenType::Rbracket);

		left = std::make_unique<IndexExpression>(
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
		case TokenType::Lsquirly:
			lexer.next();

			// empty hash
			if (lexer.get(TokenType::Rsquirly)) {
				return std::make_unique<HashLiteralExpression>();
			}

			auto firstExpression = Expression::parseStatement(lexer);
			if (lexer.get(TokenType::Colon)) {
				// hash

				std::vector<std::pair<ExpressionP, ExpressionP>> elements;
				elements.push_back({ std::move(firstExpression), Expression::parse(lexer)});

				while (lexer.get(TokenType::Comma), !lexer.get(TokenType::Rsquirly)) {
					auto key = Expression::parse(lexer);
					lexer.fetch(TokenType::Colon);
					auto value = Expression::parse(lexer);
					elements.push_back({std::move(key), std::move(value)});
				}
				return std::make_unique<HashLiteralExpression>(std::move(elements));
			}
			else {
				std::vector<ExpressionP> statements;
				statements.push_back(std::move(firstExpression));
				while (lexer.get(TokenType::Semicolon), !lexer.get(TokenType::Rsquirly))
					statements.push_back(Expression::parseStatement(lexer));

				return (statements.size() == 1)
					? std::move(statements[0])
					: std::make_unique<BlockStatement>(std::move(statements));
			}

			break;
	}

	return parseCall(lexer);
}

ExpressionP Expression::parseCall(Lexer& lexer)
{
	auto prefix = Expression::parseGrouped(lexer);
	if (!prefix || !lexer.get(TokenType::Lparen))
		return prefix;

	std::vector<ExpressionP> args;
	while (lexer.get(TokenType::Comma), !lexer.get(TokenType::Rparen))
		args.push_back(Expression::parse(lexer));

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
			auto identifier = Identifier{token.literal};
			lexer.next();
			return std::make_unique<IdentifierExpression>(identifier);
		}
		case TokenType::Integer:
		{
			uint32_t value = 0;
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

		case TokenType::Dollar:
			lexer.next();
			const auto& binaryToken = lexer.peek();
			const auto binaryTokenType = binaryToken.type;
			switch (binaryTokenType) {
				case TokenType::Asterisk:
				case TokenType::Slash:
				case TokenType::Percent:
				case TokenType::Plus:
				case TokenType::Minus:
				case TokenType::BitAnd:
				case TokenType::BitOr:
				case TokenType::BitEor:
				case TokenType::Lt:
				case TokenType::Gt:
				case TokenType::Le:
				case TokenType::Ge:
				case TokenType::Eq:
				case TokenType::Not_eq:
				case TokenType::And:
				case TokenType::Or:
					lexer.next();
					return std::make_unique<IdentifierExpression>(std::to_string(binaryTokenType));
				default:
					throw std::runtime_error("unexpected infix operator: " + std::to_string(binaryToken));
			}
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
	return fn.call(left->eval(env), right->eval(env));
}

void BinaryExpression::print(std::ostream& os) const
{
	os << *left << fn.name << *right;
}


// UnaryExpression

Value UnaryExpression::eval(EnvironmentP env) const
{
	auto evaluatedValue = value->eval(env);
	switch(op) {
		case TokenType::Minus:
			return Value{-evaluatedValue.get<Integer>()};
		case TokenType::Bang:
			if (!std::holds_alternative<bool>(evaluatedValue.data))
				return Value{false};
			return Value{!evaluatedValue.get<bool>()};
		case TokenType::Tilde:
			return Value{~evaluatedValue.get<Integer>()};
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
	auto [fn, closureEnv] = function->eval(env).get<BoundFunction>();
	return fn->call(closureEnv, env, arguments);
}

void CallExpression::print(std::ostream& os) const
{
	os << *function << "(";
	auto first = true;
	for (const auto& arg : arguments) {
		if (!first)
			os << ", ";
		first = false;
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
	while (lexer.get(TokenType::Comma), !lexer.get(TokenType::Rparen)) {
		auto token = lexer.fetch(TokenType::Identifier);
		parameters.push_back(std::string{token.literal});
	}

	return std::make_unique<FunctionExpression>(
		std::move(parameters),
		BlockStatement::parse(lexer)
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
		locals->set(parameterName, std::move(argumentValue));
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
	AbstractFunctionExpression::print(os);
	os << *body;
}

// BuiltinFunctionExpression

Value BuiltinFunctionExpression::call(
	EnvironmentP closureEnv,
	EnvironmentP callerEnv,
	const std::vector<ExpressionP>& arguments
) const
{
	Array argumentValues;
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
		std::cout << "WARNING: identifier '" + identifier + "' not found\n";
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
	while (lexer.get(TokenType::Comma), !lexer.get(TokenType::Rbracket))
		elements.push_back(Expression::parse(lexer));

	return std::make_unique<ArrayLiteralExpression>(std::move(elements));
}

Value ArrayLiteralExpression::eval(EnvironmentP env) const
{
	Array value;
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
		if (!first)
			os << ",";
		first = false;
		os << *element;
	}
	os << "]";
}


// IndexExpression

Value IndexExpression::eval(EnvironmentP env) const
{
	const auto& evluatedArray = array->eval(env);
	const auto& evaluatedIndex = index->eval(env);

	return std::visit(overloaded{
		[](const Array& value, const Integer indexValue) -> Value {
			if (indexValue < 0 || indexValue >= static_cast<Integer>(value.size()))
				return nil;
			return value[indexValue];
		},
		[](const String& value, const Integer indexValue) -> Value {
			if (indexValue < 0 || indexValue >= static_cast<Integer>(value.length()))
				return nil;
			return Value{value.substr(indexValue, 1)};	// TODO: 'char' type?
		},
		[&evaluatedIndex](const Hash& value, const auto& indexValue) -> Value {
			if (const auto iter = value.find(evaluatedIndex); iter != value.end())
				return iter->second;
			return nil;
		},
		[](const auto& value, auto& indexValue) -> Value {
			throw std::runtime_error("Error: can't index into ");// + std::to_string(value));
		}
	}, evluatedArray.data, evaluatedIndex.data);
}

void IndexExpression::print(std::ostream& os) const
{
	os << array << "[" << index << "]";
}


// HashLiteralExpression

Value HashLiteralExpression::eval(EnvironmentP env) const
{
	Hash hash{};

	for (const auto& [key, value] : elements) {
		auto keyValue = key->eval(env);
		auto valueValue = value->eval(env);
		hash.emplace(std::move(keyValue), std::move(valueValue));
	}

	return Value{std::move(hash)};
}

void HashLiteralExpression::print(std::ostream& os) const
{
	os << "{";
	bool first = true;
	for (const auto& [key, value] : elements) {
		if (!first)
			os << ",";
		first = false;
		os << key << "," << value;
	}
	os << "}";
	//os << value;
}


