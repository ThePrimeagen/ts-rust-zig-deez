#pragma once

#include "ast/object.hh"
#include "vm/code.hh"
#include "vm/symbol_table.hh"

#include <array>
#include <cassert>

struct Bytecode;

struct GlobalData {

	static constexpr size_t globalSize = 0xFF;
	std::array<Object, globalSize> globals;
	std::vector<Object> constants;
	SymbolTable symbolTable;

	GlobalData();
	~GlobalData() = default;

	// Kind of a hack to ge tto the globals in builtin functions...
	// but during tests, the globals are not really global
	static GlobalData *getLastGlobals() {
		assert(lastGlobals != nullptr);
		return lastGlobals;
	}
private:
	static GlobalData *lastGlobals;
};

//using global_data_ptr = std::shared_ptr<GlobalData>;

struct EmittedInstruction {
	OpCode code;
	size_t position;

	EmittedInstruction() : code(OpPop), position(~0) {}
};

class Compiler {
	Instructions instructions;
	GlobalData *globals;

	std::unique_ptr<SymbolTable> localScope;

	EmittedInstruction lastInstruction;
	EmittedInstruction previousInstruction;

	inline void setLastInstruction(OpCode op, size_t pos) {
		previousInstruction = std::move(lastInstruction);
		lastInstruction.code = op;
		lastInstruction.position = pos;
	}

	inline SymbolTable *getSymbolTable() {
		if (localScope) return localScope.get();
		else return &globals->symbolTable;
	}

public:

	friend struct Bytecode;
	
	explicit Compiler(GlobalData *globals_) : globals(globals_) {}
	// We don't enter scopes by keeping instructinos etc on a stack internally.
	// instead, global data also shared with VM and across lines in REPL are in an external struct
	// entering scopes is done by creating a Compiler on the stack and sharing the globals data.
	explicit Compiler(Compiler &parent)
		: globals(parent.globals)
		, localScope(std::make_unique<SymbolTable>(parent.getSymbolTable()))
		{}

	inline size_t add_constant(Object &&obj) {
		globals->constants.push_back(std::move(obj));
		return globals->constants.size() - 1;
	}

	inline size_t instructionSize() {
		return instructions.size();
	}
	inline const EmittedInstruction &getLastInstruction() {
		return lastInstruction;
	}
	inline const EmittedInstruction &getPreviousInstruction() {
		return previousInstruction; 
	}

	inline size_t emit(OpCode op, std::initializer_list<std::int64_t> operands) {
		size_t pos = instructions.make_code(op, operands);
		setLastInstruction(op, pos);
		return pos;
	}
	inline size_t emit(OpCode op) {
		size_t pos = instructions.make_code(op);
		setLastInstruction(op, pos);
		return pos;
	}

	inline bool lastInstructionIs( OpCode op) {
		if (instructions.size() == 0) 
			return false;
		return lastInstruction.code == op;
	}

	inline void removeLastInstruction() {
		instructions.contents.erase(instructions.contents.begin() + lastInstruction.position, instructions.contents.end());
		 lastInstruction= std::move(previousInstruction);
	}

	inline void replaceInstruction(size_t pos, Instructions instruction) {
		Instructions::iterator iter = instructions.begin()+pos;
		for (std::uint8_t b : instruction)
			*(iter++) = b;
	}

	inline void replaceLastPopWithReturn() {
		size_t last_pos = lastInstruction.position;
		replaceInstruction(last_pos, Instructions(OpReturnValue));
		lastInstruction.code = OpReturnValue;
	}

	inline void changeOperand(size_t pos, std::initializer_list<std::int64_t> operands) {
		OpCode op = (OpCode)instructions[pos];
		Instructions replacement(op, operands);
		replaceInstruction(pos, replacement);
	}


	inline const Symbol &define(const std::string &name) {
		return getSymbolTable()->define(name);
	}

	inline const Symbol &defineFunction(const std::string &name) {
		return getSymbolTable()->defineFunction(name);
	}
	inline std::optional<Symbol> resolve(const std::string &name) {
		return getSymbolTable()->resolve(name);
	}

	inline Instructions &&extractInstructions() {
		return std::move(instructions);
	}

	inline size_t countLocals() {
		if (localScope) return localScope->size();
		else return 0;
	}

	inline const std::vector<Symbol> &get_free() {
		return localScope->freeSymbols;
	}
};

struct Bytecode {
	Instructions instructions;
	GlobalData *globals;

	Bytecode(Compiler &&compiler)
		: instructions(std::move(compiler.instructions))
		, globals(std::move(compiler.globals))
	{}
};