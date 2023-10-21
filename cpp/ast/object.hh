#pragma once

#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <functional>
#include <variant>

#include "vm/code.hh"

// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

struct Object;

class Environment
{
	std::unordered_map<std::string, Object> store;
	Environment *outer;

	// Managing the circular dependencies of enviromments correctly needs some very clever solution.
	// (Think of a function/closure referencing the environment that it is itself contained in.) 
	// Since I don't have one at the moment, we just keep the enviromments around until the end of the program.
	// This means that the VM based solution is also more memory efficient...
	std::vector<std::unique_ptr<Environment>> sub_envs;
public:
	Environment() : outer(nullptr) {};
	Environment(Environment *o) : outer(o) {};
	Environment(const Environment &) = default;
	Environment(Environment &&) = default;
	virtual ~Environment() = default;
	bool isEmpty() {
		return !outer && store.empty();
	}
	std::optional<Object> get(const std::string &name);
	Object &set(const std::string &name, const Object &object);

	Environment *get_sub_env(Environment *outer) {
		sub_envs.push_back(std::make_unique<Environment>(outer));
		return sub_envs.back().get();
	}
};
using env_ptr = Environment*;

struct BlockStatement;

struct Function {
	std::shared_ptr<std::vector<std::string>> parameters;
	std::shared_ptr<BlockStatement> body;
	env_ptr env = nullptr;

	~Function() = default;
	std::string to_string() const;

	inline size_t paramCount() { return parameters->size(); }

	// Helper method to call the function from a builtin command, see e.g. implementation of sort
	Object call(std::initializer_list<Object> args);
};
using func_ptr = std::shared_ptr<Function>;
inline func_ptr make_function() { return std::make_shared<Function>(); }

struct CompiledFunction
{
	Instructions instructions;
	size_t numLocals;
	size_t numParameters;

	CompiledFunction()
		: numLocals(0)
		, numParameters(0)
	{}
	
	CompiledFunction(Instructions &&instr, size_t nLocals, size_t nParams) 
		: instructions(instr)
		, numLocals(nLocals)
		, numParameters(nParams)
	{}

	std::string to_string()
	{
		return "fn() {\n" + instructions.to_string() + '}';
	}
};
using comp_func_ptr = std::shared_ptr<CompiledFunction>;
inline comp_func_ptr make_compiled_function() { return std::make_shared<CompiledFunction>(); }
inline comp_func_ptr make_compiled_function(Instructions &&instrs, size_t nLocals, size_t nParams) { 
	return std::make_shared<CompiledFunction>(std::move(instrs), nLocals, nParams);
}
inline comp_func_ptr make_compiled_function(std::initializer_list<Instructions> instrs, size_t nLocals, size_t nParams) {
	return std::make_shared<CompiledFunction>(Instructions(instrs.begin(), instrs.end()), nLocals, nParams);
}

struct Closure {
	comp_func_ptr fn;
	std::vector<Object> free;

	Closure(comp_func_ptr f, size_t nFree)
		: fn(f)
	{
		free.resize(nFree);
	}

	std::string to_string();
};

using closure_ptr = std::shared_ptr<Closure>;
inline closure_ptr make_closure(comp_func_ptr fn, size_t nFree) { return std::make_shared<Closure>(fn,nFree); }
inline closure_ptr make_closure(Instructions &&instrs, size_t nLocals, size_t nParams,size_t nFree) {
	return std::make_shared<Closure>(
		std::make_shared<CompiledFunction>(std::move(instrs), nLocals, nParams), 
		nFree);
}
inline closure_ptr make_closure(std::initializer_list<Instructions> instrs, size_t nLocals, size_t nParams, size_t nFree) {
	return std::make_shared<Closure>(
		std::make_shared<CompiledFunction>(Instructions(instrs.begin(), instrs.end()), nLocals, nParams), 
		nFree);
}


using BuiltinFunctionParameters = const std::vector<Object> &;
typedef Object(*builtin_ptr)(BuiltinFunctionParameters);

struct Error {
	std::string message;
	Error(const std::string &msg) : message(msg) {}
};

struct ObjectHasher {
	size_t operator()(const Object &obj) const;
};

using Array = std::vector<Object>;
using array_ptr = std::shared_ptr<Array>;
inline array_ptr make_array() { return std::make_shared<Array>(); }
inline array_ptr make_array(std::initializer_list<Object> elems) { return std::make_shared<Array>(elems); }

using HashMap = std::unordered_map<Object, Object, ObjectHasher>;
using hashmap_ptr = std::shared_ptr<HashMap>;
inline hashmap_ptr make_hash() { return std::make_shared<HashMap>(); }
inline hashmap_ptr make_hash(std::initializer_list<HashMap::value_type> elems) { return std::make_shared<HashMap>(elems); }

class Object {
	using Data = std::variant<std::monostate, Error, bool, std::int64_t, double, std::string, func_ptr, hashmap_ptr, builtin_ptr, array_ptr, comp_func_ptr, closure_ptr >;
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
	inline constexpr bool isValid() const { return data.index() > 1; }
	inline constexpr size_t index() const { return data.index(); }
	template<typename T>
	inline constexpr bool is_a() const {
		return std::holds_alternative<T>(data);
	}
	template<typename T>
	inline constexpr T get() const {
		return std::get<T>(data);
	}
	template<typename T>
	inline constexpr T &get() {
		return std::get<T>(data);
	}

	// OCL: I do not like this, but that is what the language definition says...
	inline constexpr bool getBoolean() const
	{
		if (std::holds_alternative<std::monostate>(data))
			return false;
		else if (std::holds_alternative<bool>(data))
			return std::get<bool>(data);
		else return true;
	}
	bool is_hashable() const;

	bool operator==(const Object &other) const;
	bool operator!=(const Object &other) const;
	bool operator<(const Object &other) const;
	bool operator>(const Object &other) const;

	std::string to_string() const;
	std::string type_name() const;
	size_t hash() const;
};
