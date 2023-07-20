#pragma once

#include "lexer/lexer.hh"
#include "ast/object.hh"
#include "vm/compiler.hh"

#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <format>
#include <functional>

struct Node {
	// Since our tokens contain string_views, we cannot usefully store them,
	// and we won't be using the tokens for anything here directly.
	// Some expressions may need the token type, others the value, those will handle it as needed.

	virtual std::string to_string() const = 0;
	virtual Object eval(env_ptr env) = 0;
	virtual std::optional<std::string> compile(Compiler &compiler) = 0;
	virtual ~Node() = default;
};

struct Statement : public Node {};
struct Expression : public Node {};
using stmt_ptr = std::unique_ptr<Statement>;
using expr_ptr = std::unique_ptr<Expression>;

struct Identifier : public Expression
{
	std::string name;

	explicit inline Identifier(std::string_view n) : name(n) {}
	virtual ~Identifier() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct Program : public Statement
{
	std::vector<stmt_ptr> statements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct BlockStatement : public Statement {
	std::vector<stmt_ptr> statements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct IntegerLiteral : public Expression {
	std::int64_t value;
	
	explicit inline IntegerLiteral(std::int64_t val) : value(val) {};
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct FloatingPointLiteral : public Expression {
	double value;
	
	explicit inline FloatingPointLiteral(double val) : value(val) {};
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct BooleanLiteral : public Expression {
	bool value;

	explicit inline BooleanLiteral(bool val) : value(val) {};
	virtual ~BooleanLiteral() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct PrefixExpression : public Expression {
	token_type type = token_type::Illegal;
	expr_ptr right;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct InfixExpression : public Expression {
	expr_ptr left;
	token_type type = token_type::Illegal;
	expr_ptr right;

	const char *operator_to_string() const;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct IfExpression : public Expression {
	expr_ptr condition;
	std::unique_ptr<BlockStatement> consequence;
	std::unique_ptr<BlockStatement> alternative;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct FunctionLiteral : public Expression {
	std::shared_ptr<std::vector<std::string>> parameters;
	std::shared_ptr<BlockStatement> body;
	std::string name;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct CallExpression : public Expression {
	expr_ptr function;
	std::vector<expr_ptr> arguments;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct LetStatement : public Statement {
	std::unique_ptr<Identifier> name;
	expr_ptr value;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct ReturnStatement : public Statement {
	expr_ptr value;

	inline explicit ReturnStatement(expr_ptr &&val) : value(std::move(val)) {}
	virtual ~ReturnStatement() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct ExpressionStatement : public Statement {
	expr_ptr value;

	explicit inline ExpressionStatement(expr_ptr &&v) : value(std::move(v)) {}
	virtual ~ExpressionStatement() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct StringLiteral : public Expression
{
	std::string value;

	explicit inline StringLiteral(std::string_view str) : value(str) {}
	virtual ~StringLiteral() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct ArrayLiteral : public Expression {
	std::vector<expr_ptr> elements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct IndexExpression : public Expression {
	expr_ptr left;
	expr_ptr index;	

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};

struct HashLiteral : public Expression
{
	std::vector<std::pair<expr_ptr, expr_ptr>> elements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
	std::optional<std::string> compile(Compiler &compiler) override;
};
