#pragma once

#include <memory>
#include <vector>

#include "token.hpp"
#include "expression.hpp"

class Lexer;

using Statement = Expression;
using StatementP = ExpressionP;


struct LetStatement : public Statement
{
	LetStatement(Identifier name, ExpressionP&& value);

	static StatementP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
	virtual Value eval(EnvironmentP env) const;

private:
	const Identifier name;
	const ExpressionP value;
};

struct ReturnStatement : public Statement
{
	ReturnStatement(ExpressionP&& value);

	static StatementP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
	virtual Value eval(EnvironmentP env) const;

private:
	const ExpressionP value;
};

struct IfStatement : public Statement
{
	IfStatement(ExpressionP&& condition, StatementP&& consequence, StatementP&& alternative);

	static StatementP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
	virtual Value eval(EnvironmentP env) const;

private:
	const ExpressionP condition;
	const StatementP consequence;
	const StatementP alternative;
};

struct BlockStatement : public Statement
{
	BlockStatement(std::vector<StatementP>&& statements);

	static StatementP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
	virtual Value eval(EnvironmentP env) const;

private:
	std::vector<StatementP> statements;
};
