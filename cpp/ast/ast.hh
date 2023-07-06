#pragma once

#include "lexer/lexer.hh"
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <format>
#include <functional>

// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

struct Object;

class Environment
{
	std::unordered_map<std::string, Object> store;
	std::shared_ptr<Environment> outer;
public:
	Environment() : outer(nullptr) {};
	Environment(std::shared_ptr<Environment> o) : outer(o) {};
	Environment(const Environment &) = default;
	Environment(Environment &&) = default;
	
	// Copies the contents of another environment into this one. 
	// Needed for closures to prevent circular ownership that shared pointer cannot resolve.
	// Also probably more what is wanted, so one function cannot modify the contents of another functions closure.
	inline Environment operator=(std::shared_ptr<Environment> other) {
		store = other->store;
		outer = other->outer;
		return *this;
	}
	virtual ~Environment() = default;
	bool isEmpty() {
		return !outer && store.empty();
	}
	std::optional<Object> get(const std::string &name);
	Object &set(const std::string &name, const Object &object);
};
using env_ptr = std::shared_ptr<Environment>;

struct BlockStatement;

struct Function {
	std::shared_ptr<std::vector<std::string>> parameters;
	std::shared_ptr<BlockStatement> body; // This is special because it will be stored elsewhereas well!
	env_ptr env;
	//std::weak_ptr<Environment> env;
	//Environment env;

	virtual ~Function() = default;

	// Needed for variant in Object - presume functions are unique so compare by pointer
	bool operator==(const Function &other) const { return  this == &other; };
	bool operator!=(const Function &other) const { return  this != &other; };
	bool operator<(const Function &other) const { return  this < &other; };
	bool operator>(const Function &other) const { return  this > &other; };

	std::string to_string() const;

	inline size_t paramCount() {
		return parameters->size();
	}

	// Helper method to call the function from a builtin command, see e.g. implementation of sort
	Object call(std::initializer_list<Object> args);
};
using func_ptr = std::shared_ptr<Function>;
inline func_ptr make_function() { return std::make_shared<Function>(); }


using BuiltinFunctionParameters = const std::vector<Object> &;
typedef Object(*builtin_ptr)(BuiltinFunctionParameters);

struct Error {
	std::string message;
	Error(const std::string &msg) : message(msg) {}

	// Needed for variant in Object
	bool operator==(const Error &) const { return  false; };
	bool operator!=(const Error &) const { return  false; };
	bool operator<(const Error &) const { return  false; };
	bool operator>(const Error &) const { return  false; };
};

struct ObjectHasher {
	size_t operator()(const Object &obj) const;
};

using Array = std::vector<Object>;
using array_ptr = std::shared_ptr<Array>;
inline array_ptr make_array() { return std::make_shared<Array>();  }

using HashMap = std::unordered_map<Object, Object, ObjectHasher>;
using hashmap_ptr = std::shared_ptr<HashMap>;
inline hashmap_ptr make_hash() { return std::make_shared<HashMap>(); }

class Object {
	using Data = std::variant<std::monostate, Error, bool, std::int64_t, std::string, func_ptr, hashmap_ptr, builtin_ptr, array_ptr>;
	Data data;
public:
	bool is_returned = false; // if it would have a getter and a setter, just make it public...

	Object() = default;
	Object(const Object &) = default;
	Object(Object &&) = default;
	template<typename T>
	Object(T value) : data(value) {}
	Object(char c) : data(std::string(1, c)) {};
	~Object() = default;

	Object &operator=(const Object &) = default;
	Object &operator=(Object &&) = default;

	static Object make_error(const std::string &msg) {
		Object result(std::move(Error(msg)));
		return result;
	}

	inline constexpr bool isError() const { return std::holds_alternative<Error>(data); };
	inline constexpr bool isValid() const { return data.index() > 1; };
	inline constexpr size_t index() const { return data.index(); };


	template<typename T>
	inline constexpr bool is_a() const {
		return std::holds_alternative<T>(data);
	}

	template<typename T>
	inline constexpr T get() const {
		return std::get<T>(data);
	}

	// OCL: I do not like this, but that is what the language definition says...
	inline constexpr std::optional<bool> getBoolean() const
	{
		if (std::holds_alternative<bool>(data))
			return std::get<bool>(data);
		else if (std::holds_alternative<std::int64_t>(data))
			return std::get<std::int64_t>(data) > 0;
		else return std::nullopt;
	}

	inline constexpr bool operator==(const Object &other) const { return data == other.data; }
	inline constexpr bool operator!=(const Object &other) const { return data != other.data; }
	inline constexpr bool operator<(const Object &other) const { return data < other.data; }
	inline constexpr bool operator>(const Object &other) const { return data > other.data; }

	inline constexpr std::string to_string() const {
		return std::visit(overloaded{
			[](std::monostate) { return std::string("null"); },
			[](const Error &arg) { return arg.message; },
			[](bool arg) { return std::string(arg ? "true" : "false"); },
			[](int64_t arg) { return std::to_string(arg); },
			[](const std::string &arg) { return arg; },
			[](const func_ptr &arg) { return arg->to_string(); },
			[](const hashmap_ptr &arg) { return std::string("todo"); },
			[](const builtin_ptr &arg) { return std::string("<Builtin Function>"); },
			[](const array_ptr &arg) {
				std::string result;
				result.reserve(16 * arg->size());
				result += '[';
				auto iter = arg->begin();
				if (iter != arg->end()) {
					result += iter->to_string();
					++iter;
					for (; iter != arg->end(); ++iter) {
						result += ", " + iter->to_string();
					}
				}
				result += ']';
				return result;
				}
		}, data);
	}

	inline constexpr std::string type_name() const {
		return std::visit(overloaded{
			[](std::monostate arg) { return std::string("NIL"); },
			[](const Error &arg) { return std::string("ERROR"); },
			[](bool arg) { return std::string("BOOLEAN"); },
			[](int64_t arg) { return std::string("INTEGER"); },
			[](const std::string &arg) { return  std::string("STRING"); },
			[](const func_ptr &arg) { return std::string("FUNCTION"); },
			[](const hashmap_ptr &arg) { return std::string("HASHMAP"); },
			[](const builtin_ptr &arg) { return std::string("BUILTIN"); },
			[](const array_ptr &arg) { return std::string("ARRAY"); }
		}, data);
	}

	inline constexpr size_t hash() const {
		return std::visit(overloaded{
			[](std::monostate arg) { return (size_t)0; },
			[](const Error &arg) { return std::hash<std::string>()(arg.message); },
			[](bool arg) { return std::hash<bool>()(arg); },
			[](int64_t arg) { return std::hash<std::int64_t>()(arg); },
			[](const std::string &arg) { return  std::hash<std::string>()(arg); },
			[](const func_ptr &arg) { return std::hash<func_ptr>()(arg); },
			[](const hashmap_ptr &arg) { return std::hash<hashmap_ptr>()(arg); /*TODO?*/ },
			[](const builtin_ptr &arg) { return std::hash<size_t>()((size_t)&arg); },
			[](const array_ptr &arg) { 
				size_t seed = 0xC0FFEE; // See hash_combine() from boost
				for (const Object &o : *arg) seed ^= o.hash() + 0x9e3779b9 + (seed << 6) + (seed >> 2);
				return seed; 
			}
		}, data);
	}
};


struct Node {
	// Since our tokens contain string_views, we cannot usefully store them,
	// and we won't be using the tokens for anything here directly.
	// Some expressions may need the token type, others the value, those will handle it as needed.

	virtual std::string to_string() const = 0;
	virtual Object eval(env_ptr env) = 0;
	virtual ~Node() = default;
};

struct Statement : public Node {};
struct Expression : public Node {};
using stmt_ptr = std::unique_ptr<Statement>;
using expr_ptr = std::unique_ptr<Expression>;

struct Identifier : public Expression
{
	std::string name;

	inline Identifier(std::string_view n) : name(n) {}
	virtual ~Identifier() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct Program : public Statement
{
	std::vector<stmt_ptr> statements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct BlockStatement : public Statement {
	std::vector<stmt_ptr> statements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct IntegerLiteral : public Expression {
	std::int64_t value;
	
	inline IntegerLiteral(std::int64_t val) : value(val) {};
	virtual ~IntegerLiteral() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct BooleanLiteral : public Expression {
	bool value;

	inline BooleanLiteral(bool val) : value(val) {};
	virtual ~BooleanLiteral() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct PrefixExpression : public Expression {
	token_type type = token_type::Illegal;
	expr_ptr right;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct InfixExpression : public Expression {
	expr_ptr left;
	token_type type = token_type::Illegal;
	expr_ptr right;

	const char *operator_to_string() const;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct IfExpression : public Expression {
	expr_ptr condition = nullptr;;
	std::unique_ptr<BlockStatement> consequence;
	std::unique_ptr<BlockStatement> alternative;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct FunctionLiteral : public Expression {
	std::shared_ptr<std::vector<std::string>> parameters;
	std::shared_ptr<BlockStatement> body;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct CallExpression : public Expression {
	expr_ptr function = nullptr;
	std::vector<expr_ptr> arguments;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct LetStatement : public Statement {
	std::unique_ptr<Identifier> name;
	expr_ptr value;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct ReturnStatement : public Statement {
	expr_ptr value;

	ReturnStatement(expr_ptr &&val) : value(std::move(val)) {}
	virtual ~ReturnStatement() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct ExpressionStatement : public Statement {
	expr_ptr value;

	ExpressionStatement(expr_ptr &&v) : value(std::move(v)) {}
	virtual ~ExpressionStatement() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct StringLiteral : public Expression
{
	std::string value;

	inline StringLiteral(std::string_view str) : value(str) {}
	virtual ~StringLiteral() = default;
	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct ArrayLiteral : public Expression {
	std::vector<expr_ptr> elements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct IndexExpression : public Expression {
	expr_ptr left;
	expr_ptr index;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};

struct HashLiteral : public Expression
{
	std::vector<std::pair<expr_ptr, expr_ptr>> elements;

	std::string to_string() const override;
	Object eval(env_ptr env) override;
};
