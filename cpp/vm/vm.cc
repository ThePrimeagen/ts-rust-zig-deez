#include "vm/vm.hh"
#include <format>

extern std::vector<std::pair<std::string, builtin_ptr>> builtin_defs;

void SwitchVM::pushFrame(SwitchVM::Frame &&frame) {
	frames.push_back(std::move(frame));
	currentFrame = &frames.back();
}

SwitchVM::Frame SwitchVM::popFrame() {
	Frame frame = std::move(frames.back());
	frames.pop_back();
	currentFrame = &frames.back();
	return frame;
}

std::optional<std::string> SwitchVM::run(Bytecode &&code) {
	Frame mainFrame(std::move(make_closure(std::move(code.instructions), 0, 0, 0)), 0U);
	pushFrame(std::move(mainFrame));
	sp = 0; // A VM instance may be used multiple times ...
	return main_loop();
}

/*
	Stack Layout for sunction call:
	Callee
	Arg1
	...
	ArgN

 */

std::optional<std::string> SwitchVM::run(closure_ptr closure, std::initializer_list<Object> args) {
	// First isntruciton will be ignored, as ip initialized to first and incremented after return from second frame.
	pushFrame(Frame(std::move(make_closure({Instructions(OpPop),Instructions(OpPop)}, 0, 0, 0)), 0)); 
	pushFrame(Frame(closure, 1));
	sp = 0; // A VM instance may be used multiple times ...
	stack[sp++] = closure;
	for (const Object &arg : args)
		stack[sp++] = arg;
	return main_loop();
}

std::optional<std::string> SwitchVM::main_loop() {
	while (currentFrame->ip != currentFrame->cl->fn->instructions.end())
	{
		OpCode op = (OpCode)*currentFrame->ip;
		switch (op)
		{
		case OpConstant:
		{
			++currentFrame->ip;
			size_t constIndex = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			stack[sp++] = global_data->constants[constIndex];
			break;
		}
		case OpPop:
			--sp;
			break;
		case OpAdd:
			// We could pop into temporaries, then push the result... but why waste cycles
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() += stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else if (stack[sp - 2].is_a<std::string>() && stack[sp - 1].is_a<std::string>()) {
				stack[sp - 2].get<std::string>() += stack[sp - 1].get<std::string>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpSub:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() -= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpMul:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() *= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpDiv:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() /= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpOr:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() |= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpXor:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() ^= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpAnd:
			if (stack[sp - 2].is_a<std::int64_t>() && stack[sp - 1].is_a<std::int64_t>()) {
				stack[sp - 2].get<std::int64_t>() &= stack[sp - 1].get<std::int64_t>();
				--sp;
			}
			else goto binary_type_error;
			break;
		case OpTrue:
			stack[sp++] = true;
			break;
		case OpFalse:
			stack[sp++] = false;
			break;
		case OpEqual:
			stack[sp - 2]= Object(stack[sp - 2] == stack[sp - 1]);
			--sp;
			break;
		case OpNotEqual:
			stack[sp - 2] = Object(stack[sp - 2] != stack[sp - 1]);
			--sp;
			break;
		case OpGreaterThan:
			stack[sp - 2] = Object(stack[sp - 2] > stack[sp - 1]);
			--sp;
			break;
		case OpBang:
			if (stack[sp - 1].getBoolean()) stack[sp - 1] = false;
			else stack[sp - 1] = true;
			break;
		case OpMinus:
			if (stack[sp - 1].is_a<std::int64_t>()) 
				stack[sp - 1] = -stack[sp - 1].get<std::int64_t>();
			else goto unary_type_error;
			break;
		case OpTilde:
			if (stack[sp - 1].is_a<std::int64_t>())
				stack[sp - 1] = ~stack[sp - 1].get<std::int64_t>();
			else goto unary_type_error;
			break;
		case OpJump:
		{
			currentFrame->ip++;
			size_t pos = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			currentFrame->ip = currentFrame->cl->fn->instructions.begin() + pos - 1;
			break;
		}
		case OpJumpNotTruthy:
		{
			++currentFrame->ip;
			size_t pos = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			bool do_jump = !stack[--sp].getBoolean();
			if (do_jump)
				currentFrame->ip = currentFrame->cl->fn->instructions.begin() + pos - 1;
			break;
		}
		case OpJumpTruthy:
		{
			++currentFrame->ip;
			size_t pos = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			bool do_jump = stack[--sp].getBoolean();
			if (do_jump)
				currentFrame->ip = currentFrame->cl->fn->instructions.begin() + pos - 1;
			break;
		}
		case OpNull:
			stack[sp++] = Object();
			break;
		case OpSetGlobal:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			global_data->globals[index] = stack[--sp];
			break;
		}
		case OpGetGlobal:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			stack[sp++]= global_data->globals[index];
			break;
		}
		case OpArray:
		{
			++currentFrame->ip;
			size_t elem_count = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			array_ptr result = make_array();
			result->reserve(elem_count);
			for (size_t pos = sp - elem_count; pos != sp; ++pos)
				result->emplace_back(std::move(stack[pos]));
			sp -= elem_count;
			stack[sp++] = std::move(result);
			break;
		}
		case OpHash:
		{
			++currentFrame->ip;
			size_t elem_count = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			hashmap_ptr result = make_hash();
			result->reserve(elem_count);
			for (size_t pos = sp - elem_count; pos != sp; pos += 2)
				result->insert(std::make_pair(std::move(stack[pos]), std::move(stack[pos + 1])));
			sp -= elem_count;
			stack[sp++] = std::move(result);
			break;
		}
		case OpIndex:
			if (stack[sp - 2].is_a<array_ptr>() && stack[sp - 1].is_a<std::int64_t>()) {
				array_ptr array = stack[sp - 2].get<array_ptr>();
				std::int64_t index = stack[sp - 1].get<std::int64_t>();
				if (index < array->size())
					stack[sp - 2] = (*array)[index];
				else
					stack[sp - 2] = Object();
				--sp;
			}
			else if (stack[sp - 2].is_a<hashmap_ptr>() && stack[sp - 1].is_hashable()) {
				hashmap_ptr hash = stack[sp - 2].get<hashmap_ptr>();
				auto iter = hash->find(stack[sp - 1]);
				if (iter != hash->end())
					stack[sp - 2] = iter->second;
				else
					stack[sp - 2] = Object();
				--sp;
			}
			else
				return std::format("Index operator not supported for {}", stack[sp - 2].type_name());
			break;
		case OpCall:
		{
			++currentFrame->ip;
			size_t numArgs = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			if (stack[sp - 1 - numArgs].is_a<closure_ptr>()) {
				closure_ptr cl = stack[sp - 1 - numArgs].get<closure_ptr>();
				if (numArgs != cl->fn->numParameters)
					return std::format("Wrong number of arguments, expected {} but got {}", cl->fn->numParameters, numArgs);
				pushFrame(Frame(cl, sp - numArgs));
				sp += cl->fn->numLocals;
				continue; // Skip incrementing IP when starting on new frame!
			}
			if (stack[sp - 1 - numArgs].is_a<builtin_ptr>()) {
				builtin_ptr callee= stack[sp - 1 - numArgs].get<builtin_ptr>();
				auto from = stack.begin() + (sp - numArgs);
				auto to = stack.begin() + sp;
				std::vector<Object> args(from, to);
				sp -= numArgs + 1;
				stack[sp++] = callee(args);
				break;
			}
			else return "calling non-function";
		}
		case OpReturnValue:
		{
			Object returnValue = stack[--sp];
			sp = currentFrame->basePointer;
			popFrame();
			stack[sp - 1] = std::move(returnValue);
			break;
		}
		case OpReturn:
			sp = currentFrame->basePointer;
			popFrame();
			stack[sp - 1] = Object();
			break;
		case OpSetLocal:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			stack[currentFrame->basePointer + index] = stack[--sp];
			break;
		}
		case OpGetLocal:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			stack[sp++] = stack[currentFrame->basePointer + index];
			break;
		}
		case OpGetBuiltin:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			stack[sp++] = builtin_defs[index].second;
			break;
		}
		case OpClosure:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 2);
			++currentFrame->ip;
			size_t num_free = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			if (!global_data->constants[index].is_a<comp_func_ptr>())
				return "not a function: " + global_data->constants[index].to_string();
			closure_ptr closure = make_closure(global_data->constants[index].get<comp_func_ptr>(), num_free);
			for (size_t i = 0; i < num_free; ++i) {
				closure->free[i] = stack[sp - num_free + i];
			}
			sp -= num_free;
			stack[sp++] = closure;
			break;
		}
		case OpGetFree:
		{
			++currentFrame->ip;
			size_t index = currentFrame->cl->fn->instructions.read_int(currentFrame->ip, 1);
			stack[sp++] = currentFrame->cl->free[index];
			break;
		}
		case OpCurrentClosure:
			stack[sp++] = currentFrame->cl;
			break;
		default:
			return std::format("Unknown opcode {}", (std::uint8_t) op);
		}
		if (sp >= stackSize) return "stack overflow";
		++currentFrame->ip;
	}
	return std::nullopt;
binary_type_error:
	return std::format("Unsupported operands types for binary operation: {} {}",
		stack[sp - 2].type_name(), stack[sp - 1].type_name());
unary_type_error:
	return std::format("Unsupported operands types for unary operation: {}", stack[sp - 1].type_name());

}
