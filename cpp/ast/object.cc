#include "ast/object.hh"
#include "ast/ast.hh"

std::optional<Object> Environment::get(const std::string &name) {
	auto iter = store.find(name);
	if (iter != store.end())
		return iter->second;
	if (outer != nullptr) return outer->get(name);
	return std::nullopt;
}

Object &Environment::set(const std::string &name, const Object &object) {
	return store[name] = object;
}

size_t ObjectHasher::operator()(const Object &obj) const {
	return obj.hash();
}

std::string Function::to_string() const {
	std::string result;
	result.reserve(128);
	result += "fn(";

	auto iter = parameters->begin();
	if (iter != parameters->end()) {
		result += *(iter++);
		for (; iter != parameters->end(); ++iter)
			result += ", " + *(iter++);
	}
	result += ") " + body->to_string();
	return result;
}

Object Function::call(std::initializer_list<Object> args)
{
	env_ptr fn_env = std::make_shared<Environment>(env);
	auto pit = parameters->begin();
	for (std::initializer_list<Object>::const_iterator ait = args.begin(); ait != args.end(); ++ait, ++pit)
		fn_env->set(*pit, *ait);
	return 	body->eval(fn_env);
}


std::string Closure::to_string() {
	std::string result;
	result.reserve(256);
	result += '[';
	auto iter = free.begin();
	if (iter != free.end()) {
		result += iter->to_string();
		++iter;
		for (; iter != free.end(); ++iter) {
			result += ", ";
			result += iter->to_string();
		}
	}
	result += "]";
	result += fn->to_string();
	return result;
}

std::string Object::to_string() const {
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
			},
		[](const comp_func_ptr &arg) { return arg->to_string(); },
		[](const closure_ptr &arg) { return arg->to_string(); }
		}, data);
}

std::string Object::type_name() const {
	return std::visit(overloaded{
		[](std::monostate arg) { return std::string("NIL"); },
		[](const Error &arg) { return std::string("ERROR"); },
		[](bool arg) { return std::string("BOOLEAN"); },
		[](int64_t arg) { return std::string("INTEGER"); },
		[](const std::string &arg) { return  std::string("STRING"); },
		[](const func_ptr &arg) { return std::string("FUNCTION"); },
		[](const hashmap_ptr &arg) { return std::string("HASHMAP"); },
		[](const builtin_ptr &arg) { return std::string("BUILTIN"); },
		[](const array_ptr &arg) { return std::string("ARRAY"); },
		[](const comp_func_ptr &arg) { return std::string("COMPILED_FUNCTION"); },
		[](const closure_ptr &arg) { return std::string("CLOSURE"); }
		}, data);
}

bool Object::is_hashable() const {
	return std::visit(overloaded{
		[](std::monostate arg) { return false; },
		[](const Error &arg) { return false; },
		[](bool arg) { return true; },
		[](int64_t arg) { return true; },
		[](const std::string &arg) { return true; },
		[](const func_ptr &arg) { return false; },
		[](const hashmap_ptr &arg) { return true; },
		[](const builtin_ptr &arg) { return false; },
		[](const array_ptr &arg) { return true; },
		[](const comp_func_ptr &arg) { return false; },
		[](const closure_ptr &arg) { return false; }
		}, data);
}

size_t Object::hash() const {
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
		},
		[](const comp_func_ptr &arg) { return std::hash<size_t>()((size_t)&arg); },
		[](const closure_ptr &arg) { return std::hash<size_t>()((size_t)&arg); }
		}, data);
}

bool Object::operator==(const Object &other) const {
	if (data.index() != other.data.index()) 
		return false;
	return std::visit(overloaded{
		[&other](std::monostate arg) { return true; },
		[&other](const Error &arg) { return true; },
		[&other](bool arg) { return arg==std::get<bool>(other.data); },
		[&other](int64_t arg) { return arg == std::get<std::int64_t>(other.data); },
		[&other](const std::string &arg) { return  arg == std::get<std::string>(other.data); },
		[&other](const func_ptr &arg) { return arg.get() == std::get<func_ptr>(other.data).get(); },
		[&other](const hashmap_ptr &arg) { return *arg == *std::get<hashmap_ptr>(other.data); },
		[&other](const builtin_ptr &arg) { return *arg == *std::get<builtin_ptr>(other.data); },
		[&other](const array_ptr &arg) { return *arg == *std::get<array_ptr>(other.data); },
		[&other](const comp_func_ptr &arg) { return arg->instructions == std::get<comp_func_ptr>(other.data)->instructions;},
		[&other](const closure_ptr &arg) {
			return arg->fn->instructions == std::get<closure_ptr>(other.data)->fn->instructions
				&& arg->free == std::get<closure_ptr>(other.data)->free;
		}
		}, data);
}

bool Object::operator!=(const Object &other) const  {
	if (data.index() != other.data.index())
		return false;
	return std::visit(overloaded{
		[&other](std::monostate arg) { return false; },
		[&other](const Error &arg) { return false; },
		[&other](bool arg) { return arg != std::get<bool>(other.data); },
		[&other](int64_t arg) { return arg != std::get<std::int64_t>(other.data); },
		[&other](const std::string &arg) { return  arg != std::get<std::string>(other.data); },
		[&other](const func_ptr &arg) { return arg.get() != std::get<func_ptr>(other.data).get(); },
		[&other](const hashmap_ptr &arg) { return *arg != *std::get<hashmap_ptr>(other.data); },
		[&other](const builtin_ptr &arg) { return *arg != *std::get<builtin_ptr>(other.data); },
		[&other](const array_ptr &arg) { return *arg != *std::get<array_ptr>(other.data); },
		[&other](const comp_func_ptr &arg) { return arg.get()->instructions != std::get<comp_func_ptr>(other.data).get()->instructions; },
		[&other](const closure_ptr &arg) {
			return arg->fn->instructions != std::get<closure_ptr>(other.data)->fn->instructions
				|| arg->free != std::get<closure_ptr>(other.data)->free;
		}
		}, data);
}

bool Object::operator<(const Object &other) const {
	if (data.index() != other.data.index())
		return false;
	return std::visit(overloaded{
		[&other](std::monostate arg) { return true; },
		[&other](const Error &arg) { return true; },
		[&other](bool arg) { return arg < std::get<bool>(other.data); },
		[&other](int64_t arg) { return arg < std::get<std::int64_t>(other.data); },
		[&other](const std::string &arg) { return  arg < std::get<std::string>(other.data); },
		[&other](const func_ptr &arg) { return arg < std::get<func_ptr>(other.data); },
		[&other](const hashmap_ptr &arg) { return false; }, // unordered_map implements no less operator
		[&other](const builtin_ptr &arg) { return *arg < *std::get<builtin_ptr>(other.data); },
		[&other](const array_ptr &arg) { return *arg < *std::get<array_ptr>(other.data); },
		[&other](const comp_func_ptr &arg) { return arg.get() < std::get<comp_func_ptr>(other.data).get(); },
		[&other](const closure_ptr &arg) { return arg.get() < std::get<closure_ptr>(other.data).get(); }
		}, data);
}

bool Object::operator>(const Object &other) const {
	if (data.index() != other.data.index())
		return false;
	return std::visit(overloaded{
		[&other](std::monostate arg) { return true; },
		[&other](const Error &arg) { return true; },
		[&other](bool arg) { return arg > std::get<bool>(other.data); },
		[&other](int64_t arg) { return arg > std::get<std::int64_t>(other.data); },
		[&other](const std::string &arg) { return  arg > std::get<std::string>(other.data); },
		[&other](const func_ptr &arg) { return arg > std::get<func_ptr>(other.data); },
		[&other](const hashmap_ptr &arg) { return false; }, // unordered_map implements no greater operator
		[&other](const builtin_ptr &arg) { return *arg > *std::get<builtin_ptr>(other.data); },
		[&other](const array_ptr &arg) { return *arg > *std::get<array_ptr>(other.data); },
		[&other](const comp_func_ptr &arg) { return arg.get() > std::get<comp_func_ptr>(other.data).get(); },
		[&other](const closure_ptr &arg) { return arg.get() > std::get<closure_ptr>(other.data).get(); }
		}, data);
}