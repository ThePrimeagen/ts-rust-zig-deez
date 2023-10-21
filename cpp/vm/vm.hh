#pragma once
#include "ast/object.hh"
#include "vm/compiler.hh"
#include <vector>
#include <array>
#include <string>
#include <optional>

class SwitchVM {

	struct Frame {
		//comp_func_ptr fn;
		closure_ptr cl;
		Instructions::const_iterator ip;
		size_t basePointer;

		Frame() : basePointer(0) {}
		//Frame(comp_func_ptr f, size_t bp) : fn(f), ip(fn->instructions.begin()), basePointer(bp) {}
		Frame(closure_ptr c, size_t bp) : cl(c), ip(cl->fn->instructions.begin()), basePointer(bp) {}
		Frame(const Frame &) = default;
		Frame(Frame &&) = default;

		Frame &operator=(const Frame &) = default;
		Frame &operator=(Frame &&) = default;
	};

	static const size_t stackSize = 2048;
	// Turns out the array is allocated on the stack :-) and VS2019 complains that is too much stack used.
	//std::array<Object, StackSize> stack; 
	std::vector<Object> stack;
	size_t sp; // Stack pointer, points to next value

	GlobalData *global_data;

	std::vector<Frame> frames;
	Frame *currentFrame;
	void pushFrame(Frame &&frame);
	Frame popFrame();

	std::optional<std::string> main_loop();
public:
	// This VM uses the data in the bytecode passed, no reason for copying...
	explicit SwitchVM(GlobalData *globals)
		: sp(0)
		, global_data(globals)
		, currentFrame(nullptr)
	{
		stack.resize(stackSize);
		frames.reserve(1024);
	}

	inline Object last_popped_element() {
		return stack[sp];
	}

	// Run a script from file or repl
	std::optional<std::string> run(Bytecode &&code);
	// Run a function from (usually) a builtin function, see e.g. sort.
	std::optional<std::string> run(closure_ptr closure, std::initializer_list< Object> args);

};