#pragma once

#include <string>
#include <unordered_map>
#include <optional>
#include <format>

using SymbolScope = const char *;



struct Symbol {
	std::string name;
	SymbolScope scope;
	std::int64_t index;

	Symbol() : scope(nullptr), index(0)
	{}
	Symbol(const std::string &n, SymbolScope s, size_t i)
		: name(n), scope(s), index((std::int64_t)i)
	{ }
	Symbol(const Symbol &) = default;

	Symbol &operator=(const Symbol &) = default;
	bool operator==(const Symbol &) const = default;
	bool operator!=(const Symbol &) const = default;

	std::string to_string() const {
		return std::format("{} - {} - {}", name, scope, index);
	}
};

class SymbolTable
{
	std::unordered_map<std::string, Symbol> symbols;
	size_t numDefs = 0;; // shouldbe equal to symbols.size()?
	SymbolTable *outer;

	Symbol define_free(Symbol original);
public:
	std::vector<Symbol> freeSymbols;

	const static SymbolScope GlobalScope;
	const static SymbolScope LocalScope;
	const static SymbolScope BuiltinScope;
	const static SymbolScope FreeScope;
	const static SymbolScope FunctionScope;

	SymbolTable() : outer(nullptr) {}
	explicit SymbolTable(SymbolTable *parent) : outer(parent) {}

	using iterator = std::unordered_map<std::string, Symbol>::iterator;
	using const_iterator = std::unordered_map<std::string, Symbol>::const_iterator;

	inline const_iterator begin() const {
		return symbols.begin();
	}
	inline iterator begin() {
		return symbols.begin();
	}
	inline const_iterator end() const {
		return symbols.end();
	}
	inline iterator end() {
		return symbols.end();
	}

	const Symbol &define(const std::string &name);

	std::optional<Symbol> resolve(const std::string &name);

	inline const Symbol &defineBuiltin(size_t index, const std::string &name) {
		Symbol sym(name, BuiltinScope, index);
		return symbols[name] = sym;
	}

	inline const Symbol &defineFunction(const std::string &name) {
		Symbol sym(name, FunctionScope, 0);
		return symbols[name] = sym;
	}

	inline size_t size() {
		return symbols.size();
	}
};