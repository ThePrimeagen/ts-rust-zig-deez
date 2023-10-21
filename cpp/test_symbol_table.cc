#include "tests.hh"
#include "vm/symbol_table.hh"

void test_define() {
	std::unordered_map<std::string, Symbol> expected = {
		{ "a", {"a", SymbolTable::GlobalScope, 0}},
		{ "b", {"b", SymbolTable::GlobalScope, 1}},
		{ "c", {"c", SymbolTable::LocalScope, 0}},
		{ "d", {"d", SymbolTable::LocalScope, 1}},
		{ "e", {"e", SymbolTable::LocalScope, 0}},
		{ "f", {"f", SymbolTable::LocalScope, 1}},
	};

	TestHelper tt("SymbolTable define");

	SymbolTable global;

	const Symbol &a = global.define("a");
	if (a != expected["a"])	{
		tt.fail("expected a=", expected["a"].to_string(), " but got ", a.to_string());
		return;
	}
	const Symbol &b = global.define("b");
	if (b != expected["b"]) {
		tt.fail("expected b=", expected["b"].to_string(), " but got ", b.to_string());
		return;
	}

	SymbolTable firstLocal(&global);

	const Symbol &c = firstLocal.define("c");
	if (c != expected["c"])	{
		tt.fail("expected c=", expected["c"].to_string(), " but got ", c.to_string());
		return;
	}
	const Symbol &d = firstLocal.define("d");
	if (d != expected["d"]) {
		tt.fail("expected d=", expected["d"].to_string(), " but got ", d.to_string());
		return;
	}

	SymbolTable secondLocal(&firstLocal);

	const Symbol &e = secondLocal.define("e");
	if (e != expected["e"])	{
		tt.fail("expected e=", expected["e"].to_string(), " but got ", e.to_string());
		return;
	}
	const Symbol &f = secondLocal.define("f");
	if (f != expected["f"]) {
		tt.fail("expected f=", expected["f"].to_string(), " but got ", f.to_string());
		return;
	}

}

void test_resolve() {
	TestHelper tt("SymbolTable resolve");

	SymbolTable global;
	global.define("a");
	global.define("b");
	std::unordered_map<std::string, Symbol> expected = {
		{ "a", {"a", SymbolTable::GlobalScope, 0}},
		{ "b", {"b", SymbolTable::GlobalScope, 1}},
	};

	for (const auto &sym : expected) {
		auto found = global.resolve(sym.second.name);
		if (!found) {
			tt.fail("Could not resolve name ", sym.second.name);
			continue;
		}
		if (sym.second != *found) {
			tt.fail("Symbol found not equal to expected symbol");
			continue;
		}
	}

}

void test_resolve_local() {
	TestHelper tt("SymbolTable resolve local");

	SymbolTable global;
	global.define("a");
	global.define("b");

	SymbolTable local(&global);
	local.define("c");
	local.define("d");

	std::unordered_map<std::string, Symbol> expected = {
		{ "a", {"a", SymbolTable::GlobalScope, 0}},
		{ "b", {"b", SymbolTable::GlobalScope, 1}},
		{ "c", {"c", SymbolTable::LocalScope, 0}},
		{ "c", {"d", SymbolTable::LocalScope, 1}},
	};

	for (const auto &sym : expected) {
		auto found = local.resolve(sym.second.name);
		if (!found) {
			tt.fail("Could not resolve name ", sym.second.name);
			continue;
		}
		if (sym.second != *found) {
			tt.fail("Symbol found not equal to expected symbol");
			continue;
		}
	}

}

void test_resolve_nested_local() {
	TestHelper tt("SymbolTable resolve nested local");

	SymbolTable global;
	global.define("a");
	global.define("b");

	SymbolTable local(&global);
	local.define("c");
	local.define("d");

	SymbolTable nested_local(&local);
	nested_local.define("e");
	nested_local.define("f");


	std::pair<SymbolTable*,std::unordered_map<std::string, Symbol>> expected[] = { 
		{
			std::make_pair<SymbolTable *,std::unordered_map<std::string, Symbol>>(&local, 
					{
						{ "a", {"a", SymbolTable::GlobalScope, 0}},
						{ "b", {"b", SymbolTable::GlobalScope, 1}},
						{ "c", {"c", SymbolTable::LocalScope, 0}},
						{ "c", {"d", SymbolTable::LocalScope, 1}}
					}
				)
		},
		{
			std::make_pair<SymbolTable *,std::unordered_map<std::string, Symbol>>(&nested_local,
				{
					{ "a", {"a", SymbolTable::GlobalScope, 0}},
					{ "b", {"b", SymbolTable::GlobalScope, 1}},
					{ "e", {"e", SymbolTable::LocalScope, 0}},
					{ "f", {"f", SymbolTable::LocalScope, 1}}
				}
			)
		}
	};

	for (auto iter = std::begin(expected); iter != std::end(expected); ++iter) {
		for (const auto &sym : iter->second) {
			auto found = iter->first->resolve(sym.second.name);
			if (!found) {
				tt.fail("Could not resolve name ", sym.second.name);
				continue;
			}
			if (sym.second != *found) {
				tt.fail("Symbol found not equal to expected symbol");
				continue;
			}
		}
	}

}

void test_resolve_builtins() {
	TestHelper tt("Resolve builtins");

	SymbolTable global;
	SymbolTable firstLocal(&global);
	SymbolTable secondLocal(&firstLocal);

	std::vector<Symbol> expected{
		{"a", SymbolTable::BuiltinScope, 0},
		{"b", SymbolTable::BuiltinScope, 1},
		{"e", SymbolTable::BuiltinScope, 2},
		{"f", SymbolTable::BuiltinScope, 3}
	};

	size_t i = 0;
	for (const auto &sym : expected)
		global.defineBuiltin(i++,sym.name);

	SymbolTable *tables[] = { &global, &firstLocal, &secondLocal };
	for (auto iter = std::begin(tables); iter != std::end(tables); ++iter) {
		for (const auto &sym : expected) {
			auto found = (*iter)->resolve(sym.name);
			if (!found) {
				tt.fail("Could not resolve name ", sym.name);
				continue;
			}
			if (sym != *found) {
				tt.fail("Symbol found not equal to expected symbol");
				continue;
			}
		}
	}
}

void test_resolve_free() {
	TestHelper tt("Resolve free variables");
	SymbolTable global;
	global.define("a");
	global.define("b");

	SymbolTable firstLocal(&global);
	firstLocal.define("c");
	firstLocal.define("d");

	SymbolTable secondLocal(&firstLocal);
	secondLocal.define("e");
	secondLocal.define("f");

	struct {
		SymbolTable *table;
		std::vector<Symbol> expected;
		std::vector<Symbol> expectedFree;
	} tests[] = {
		{
			&firstLocal,
			{
				{"a", SymbolTable::GlobalScope, 0},
				{"b", SymbolTable::GlobalScope, 1},
				{"c", SymbolTable::LocalScope, 0},
				{"d", SymbolTable::LocalScope, 1},
			},
			{}
		},
		{
			&secondLocal,
			{
				{"a", SymbolTable::GlobalScope, 0},
				{"b", SymbolTable::GlobalScope, 1},
				{"c", SymbolTable::FreeScope, 0},
				{"d", SymbolTable::FreeScope, 1},
				{"e", SymbolTable::LocalScope, 0},
				{"f", SymbolTable::LocalScope, 1},
			},
			{
				{"c", SymbolTable::LocalScope, 0},
				{"d", SymbolTable::LocalScope, 1},
			}
		}
	};

	for (auto iter = std::begin(tests); iter != std::end(tests); ++iter) {
		for (const auto &sym : iter->expected) {
			auto found = iter->table->resolve(sym.name);
			if (!found) {
				tt.fail("Name not resolvable: ", sym.name);
				continue;
			}
			if (*found != sym) {
				tt.fail("Expected ", sym.name, " to resolve to ", sym.to_string(), " but got ", found->to_string());
			}
		}

		if (iter->expectedFree.size() != iter->table->freeSymbols.size()) {
			tt.fail("wrong number of free symbols. got=", iter->table->freeSymbols.size(), " want=", iter->expectedFree.size());
			continue;
		}

		for (size_t i = 0; i < iter->expectedFree.size(); ++i) {
			if (iter->table->freeSymbols[i] != iter->expectedFree[i]) {
				tt.fail("wrong free symbol. got=", iter->table->freeSymbols[i].to_string(), " want=%+v", iter->expectedFree[i].to_string());
			}
		}
	}
}

void test_unresovable_free() {
	TestHelper tt("Unresolvable free variables");
	SymbolTable global;
	global.define("a");

	SymbolTable firstLocal(&global);
	firstLocal.define("c");

	SymbolTable secondLocal(&firstLocal);
	secondLocal.define("e");
	secondLocal.define("f");

	std::vector<Symbol> expected{
		{"a", SymbolTable::GlobalScope, 0},
		{"c", SymbolTable::FreeScope, 0},
		{"e", SymbolTable::LocalScope, 0},
		{"f", SymbolTable::LocalScope, 1},
	};

	for (const auto &sym : expected) {
		auto found = secondLocal.resolve(sym.name);
		if (!found) {
			tt.fail("Name not resolvable: ", sym.name);
			continue;
		}
		if (*found != sym) {
			tt.fail("Expected ", sym.name, " to resolve to ", sym.to_string(), " but got ", found->to_string());
		}
	}

	std::vector<std::string> expectUnresolvable = { "b", "d" };

	for (const auto &name : expectUnresolvable) {
		auto found = secondLocal.resolve(name);
		if (found) {
			tt.fail("Name ", name, " resolved, but was not expected to");
		}
	}
}

void test_define_resolve_function_name() {
	TestHelper tt("Define and resolve function name");
	SymbolTable global;
	global.defineFunction("a");

	Symbol expected = { "a", SymbolTable::FunctionScope, 0 };

	auto found = global.resolve(expected.name);

	if (!found) {
		tt.fail("Name not resolvable: ", expected.name);
		return;
	}
	if (*found != expected) {
		tt.fail("Expected ", expected.name, " to resolve to ", expected.to_string(), " but got ", found->to_string());
	}


}

void test_shadow_function_name() {
	TestHelper tt("Shadow functionn name");
	SymbolTable global;
	global.defineFunction("a");
	global.define("a");

	Symbol expected = { "a", SymbolTable::GlobalScope, 0 };

	auto found = global.resolve(expected.name);

	if (!found) {
		tt.fail("Name not resolvable: ", expected.name);
		return;
	}
	if (*found != expected) {
		tt.fail("Expected ", expected.name, " to resolve to ", expected.to_string(), " but got ", found->to_string());
	}


}

void test_symbol_table() {
	std::cout << ">>> Symbol table tests <<<\n";

	test_define();
	test_resolve();
	test_resolve_local();
	test_resolve_nested_local();
	test_resolve_builtins();
	test_resolve_free(); 
	test_unresovable_free();
	test_define_resolve_function_name();
	test_shadow_function_name();

	std::cout << std::endl;
}