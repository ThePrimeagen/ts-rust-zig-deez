#pragma once

#include "lexer/lexer.hh"
#include <string>
#include <vector>
#include <memory>

struct Node {
	// Since our tokens contain string_views, we cannot usefully store them,
	// so we store the type, and where we need the indentifier, we copy it into a local string
	//token_type token;

	virtual std::string to_string() = 0;
};

struct Statement : public Node {};

struct Expression : public Node {};

using stmt_ptr = std::unique_ptr<Statement>;
using expr_ptr = std::unique_ptr<Expression>;

struct Program : public Statement
{
	std::vector<std::unique_ptr<Statement>> statements;
	std::string to_string() override;
};

struct BlockStatement : public Statement {
	std::vector<std::unique_ptr<Statement>> statements;
	
	std::string to_string() override;
};

struct Identifier : public Expression
{
	std::string name;

	inline Identifier(std::string_view n) : name(n) {}
	std::string to_string() override;
};

struct IntegerLiteral : public Expression {
	std::int64_t value;

	inline IntegerLiteral(std::int64_t val) : value(val) {};
	std::string to_string() override;
};

struct BooleanLiteral : public Expression {
	bool value;

	inline BooleanLiteral(bool val) : value(val) {};
	std::string to_string() override;
};

struct PrefixExpression : public Expression {
	token_type type;
	std::unique_ptr<Expression> right;

	std::string to_string() override;
};

struct InfixExpression : public Expression {
	std::unique_ptr<Expression> left;
	token_type type;
	std::unique_ptr<Expression> right;

	std::string to_string() override;
};

struct IfExpression : public Expression {
	expr_ptr condition;
	std::unique_ptr<BlockStatement> consequence;
	std::unique_ptr<BlockStatement> alternative;

	std::string to_string();
};

struct FunctionLiteral : public Expression {
	std::vector<std::unique_ptr<Identifier>> parameters;
	std::unique_ptr<BlockStatement> body;

	std::string to_string();;
};

struct CallExpression : public Expression {
	expr_ptr function;
	std::vector<expr_ptr> arguments;

	std::string to_string() override;
};

struct LetStatement : public Statement {
	std::unique_ptr<Identifier> name;
	std::unique_ptr<Expression> value;

	std::string to_string() override;
};

struct ReturnStatement : public Statement {
	std::unique_ptr<Expression> value;

	std::string to_string() override;
};

struct ExpressionStatement : public Statement {
	std::unique_ptr<Expression> value;

	ExpressionStatement(std::unique_ptr<Expression> &&v) : value(std::move(v)) {}
	std::string to_string() override;
};