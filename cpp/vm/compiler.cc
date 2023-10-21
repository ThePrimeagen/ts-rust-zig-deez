#include "vm/compiler.hh"

extern std::vector<std::pair<std::string, builtin_ptr>> builtin_defs;

GlobalData *GlobalData::lastGlobals = nullptr;
GlobalData::GlobalData() {
	for (size_t i = 0; i < builtin_defs.size(); ++i) {
		symbolTable.defineBuiltin(i, builtin_defs[i].first);
	}
	lastGlobals = this;
}