#include "vm/symbol_table.hh"


const SymbolScope SymbolTable::GlobalScope = "GlobalScope";
const SymbolScope SymbolTable::LocalScope = "LocalScope";
const SymbolScope SymbolTable::BuiltinScope = "BuiltinScope";
const SymbolScope SymbolTable::FreeScope = "FreeScope";
const SymbolScope SymbolTable::FunctionScope = "FunctionScope";

Symbol SymbolTable::define_free(Symbol original) {
	freeSymbols.push_back(original);
	Symbol symbol(original.name, FreeScope, freeSymbols.size() - 1);
	symbols[original.name] = symbol;
	return symbol;
}
const Symbol &SymbolTable::define(const std::string &name) {
	SymbolScope scope = GlobalScope;
	if (outer != nullptr)
		scope = LocalScope;
	Symbol sym(name, scope, numDefs++);
	return symbols[name] = sym;
}

std::optional<Symbol> SymbolTable::resolve(const std::string &name) {
	const_iterator iter = symbols.find(name);
	if (iter != symbols.end())
		return iter->second;
	if (outer == nullptr)
		return std::nullopt;

	std::optional<Symbol> found = outer->resolve(name);
	if (!found)
		return std::nullopt;

	if (found->scope == GlobalScope || found->scope == BuiltinScope)
		return found;
	return define_free(*found);

}
