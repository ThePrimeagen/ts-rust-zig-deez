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

struct StatementList : public Statement
{
	StatementList(std::vector<StatementP>&& statements)
	: statements{std::move(statements)} {}

	void print(std::ostream& str) const override;
	virtual Value eval(EnvironmentP env) const;

protected:
	std::vector<StatementP> statements;
};

struct BlockStatement : public StatementList
{
	using StatementList::StatementList;

	static StatementP parse(Lexer& lexer);
	void print(std::ostream& str) const override;
};
