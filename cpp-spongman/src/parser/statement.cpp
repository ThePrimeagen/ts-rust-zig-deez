#include "statement.hpp"
#include "expression.hpp"
#include "lexer.hh"


StatementP Statement::parseStatement(Lexer& lexer)
{
	switch (lexer.type()) {
		case TokenType::Let:      return LetStatement::parse(lexer);
		case TokenType::Return:   return ReturnStatement::parse(lexer);
		//case TokenType::Lsquirly: return BlockStatement::parse(lexer);
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
		Identifier{identifierToken.literal},
		Expression::parse(lexer)
	);
}

Value LetStatement::eval(EnvironmentP env) const
{
	env->set(name, value->eval(env));
	return nil;
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

void ReturnStatement::print(std::ostream& os) const
{
	os << "return " << *value << ";";
}


// StatementList

Value StatementList::eval(EnvironmentP env) const
{
	Value value;
	for (const auto& statement : statements) {
		value = statement->eval(env);
	}
	return value;
}

void StatementList::print(std::ostream& os) const
{
	for (const auto& statement : statements)
		os << *statement << ";\n";
}



// BlockStatement

ExpressionP BlockStatement::parse(Lexer& lexer)
{
	if (!lexer.get(TokenType::Lsquirly))
		return Expression::parseStatement(lexer);

	std::vector<ExpressionP> expressions;
	while(lexer.get(TokenType::Semicolon), !lexer.get(TokenType::Rsquirly))
		expressions.push_back(Expression::parseStatement(lexer));

	return (expressions.size() == 1)
		? std::move(expressions[0])
		: std::make_unique<BlockStatement>(std::move(expressions));
}

void BlockStatement::print(std::ostream& os) const
{
	os << "{\n";
	StatementList::print(os);
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

	auto consequence = BlockStatement::parse(lexer);
	StatementP alternative;

	if (lexer.type() == TokenType::Else) {
		lexer.next();
		alternative = BlockStatement::parse(lexer);
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
		[](Integer val)              { return val != 0; },
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
