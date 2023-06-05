#include "statement.hpp"
#include "expression.hpp"
#include "lexer.hh"


StatementP Statement::parseStatement(Lexer& lexer)
{
	switch (lexer.type()) {
		case TokenType::Let:      return LetStatement::parse(lexer);
		case TokenType::Return:   return ReturnStatement::parse(lexer);
		case TokenType::Lsquirly: return BlockStatement::parse(lexer);
	}
	return Expression::parse(lexer);
}


// LetStatement

LetStatement::LetStatement(Identifier name, ExpressionP&& value)
: name{name}, value{std::move(value)}
{
}

StatementP LetStatement::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Let);
	const auto identifierToken = lexer.fetch(TokenType::Identifier);
	lexer.fetch(TokenType::Assign);

	return std::make_unique<LetStatement>(
		identifierToken.literal,
		Expression::parse(lexer)
	);
}

Value LetStatement::eval(EnvironmentP env) const
{
	auto evaluatedValue = value->eval(env);
	env->set(name, evaluatedValue);
	return evaluatedValue;
}

void LetStatement::print(std::ostream& os) const
{
	os << "let " << name << " = " << *value << ";";
}


// ReturnStatement

ReturnStatement::ReturnStatement(ExpressionP&& value)
: value{std::move(value)}
{
}

StatementP ReturnStatement::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Return);

	return std::make_unique<ReturnStatement>(
		Expression::parse(lexer)
	);
}

Value ReturnStatement::eval(EnvironmentP env) const
{
	const auto evaluatedValue = value->eval(env);
	throw evaluatedValue;
}


// ReturnStatement

BlockStatement::BlockStatement(std::vector<StatementP>&& statements)
: statements{std::move(statements)}
{
}

void ReturnStatement::print(std::ostream& os) const
{
	os << "return " << *value << ";";
}


// BlockStatement

ExpressionP BlockStatement::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::Lsquirly);

	std::vector<ExpressionP> expressions;
	do {
		if (lexer.peekIs(TokenType::Rsquirly))
			break;
		expressions.push_back(Expression::parseStatement(lexer));
		lexer.get(TokenType::Semicolon);
	}
	while(!lexer.get(TokenType::Rsquirly));

	if (expressions.size() == 1)
		return std::move(expressions[0]);

	return std::make_unique<BlockStatement>(
		std::move(expressions)
	);
}

Value BlockStatement::eval(EnvironmentP env) const
{
	Value value;
	for (const auto& statement : statements) {
		value = statement->eval(env);
	}
	return value;
}

void BlockStatement::print(std::ostream& os) const
{
	os << "{\n";
	for (const auto& statement : statements)
		os << *statement << ";\n";
	os << "}\n";
}



// IfStatement

IfStatement::IfStatement(ExpressionP&& condition, StatementP&& consequence, StatementP&& alternative)
: condition{std::move(condition)}, consequence{std::move(consequence)}, alternative{std::move(alternative)}
{
}

StatementP IfStatement::parse(Lexer& lexer)
{
	lexer.fetch(TokenType::If);
	lexer.fetch(TokenType::Lparen);
	auto condition = Expression::parse(lexer);
	lexer.fetch(TokenType::Rparen);

	auto consequence = Statement::parseStatement(lexer);
	StatementP alternative;

	if (lexer.type() == TokenType::Else) {
		lexer.next();
		alternative = Statement::parseStatement(lexer);
	}

	return std::make_unique<IfStatement>(
		std::move(condition),
		std::move(consequence),
		std::move(alternative)
	);
}

Value IfStatement::eval(EnvironmentP env) const
{
	auto value = condition->eval(env);
	auto truthyValue = std::visit(overloaded{
		[](bool val)                 { return val; },
		[](int64_t val)              { return val != 0; },
		[](const auto& val) {
			throw std::runtime_error("invalid condition: " + std::to_string(val));
			return false;
		}
	}, value.data);

	if (truthyValue)
		return consequence->eval(env);

	if (alternative)
		return alternative->eval(env);

	return nil;
}


void IfStatement::print(std::ostream& os) const
{
	os << "if (" << *condition << ")\n";
	os << "\t" << *consequence << "\n";
	if (alternative) {
		os << "else\n";
		os << "\t" << *alternative << "\n";
	}
}
