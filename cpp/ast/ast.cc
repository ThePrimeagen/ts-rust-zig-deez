#include "ast.hh"

extern std::unordered_map<std::string, builtin_ptr> builtins;

std::optional<std::string> load_symbol(Symbol symbol, Compiler &compiler) {
	if (symbol.scope == SymbolTable::GlobalScope)
		compiler.emit(OpGetGlobal, { symbol.index });
	else if (symbol.scope == SymbolTable::LocalScope)
		compiler.emit(OpGetLocal, { symbol.index });
	else if (symbol.scope == SymbolTable::BuiltinScope)
		compiler.emit(OpGetBuiltin, { symbol.index });
	else if (symbol.scope == SymbolTable::FreeScope)
		compiler.emit(OpGetFree, { symbol.index });
	else if (symbol.scope == SymbolTable::FunctionScope)
		compiler.emit(OpCurrentClosure);
	else return "Unexpected Scope type";
	return std::nullopt;
}


std::string Program::to_string() const {
	std::string result;
	result.reserve(1024); // just some value
	for (const stmt_ptr &statement : statements) {
		result += statement->to_string();
	}
	return result;
}

Object Program::eval(env_ptr env) {
	Object result;
	for (stmt_ptr &statement : statements)
	{
		result = statement->eval(env);
		if (result.is_returned || result.isError())
			return result;
	}
	return result;
}

std::optional<std::string> Program::compile(Compiler &compiler) {
	for (stmt_ptr &statement : statements)
	{
		std::optional<std::string> result = statement->compile(compiler);
		if (result)
			return result;
	}
	return std::nullopt;
}


std::string BlockStatement::to_string() const {
	std::string result;
	result.reserve(1024); // just some value
	result += "{ ";
	for (const stmt_ptr &statement : statements) {
		result += statement->to_string();
	}
	result += " }";
	return result;
}

Object BlockStatement::eval(env_ptr env) {
	Object result;
	for (auto &statement : statements)
	{
		result = statement->eval(env);
		if (result.is_returned || result.isError())
			return result;
	}
	return result;
}

std::optional<std::string> BlockStatement::compile(Compiler &compiler) {
	for (stmt_ptr &statement : statements)
	{
		std::optional<std::string> result = statement->compile(compiler);
		if (result)
			return result;
	}
	return std::nullopt;
}

std::string Identifier::to_string() const {
	return name;
}

Object Identifier::eval(env_ptr env) {
	auto value = env->get(name);
	if (value) return *value;
	auto iter = builtins.find(name);
	if (iter != builtins.end()) return iter->second;
	return Object::make_error("identifier not found: " + name);
}

std::optional<std::string> Identifier::compile(Compiler &compiler) {
	auto symbol = compiler.resolve(name);
	if (!symbol)
		return "Undefined variable " + name;
	return load_symbol(*symbol, compiler);
};

 std::string IntegerLiteral::to_string() const {
	return std::to_string(value);
}

 Object IntegerLiteral::eval(env_ptr env)  {
	 return value;
 }

std::optional<std::string> IntegerLiteral::compile(Compiler &compiler) {
	 compiler.emit(OpConstant, { (std::int64_t)compiler.add_constant(Object(value)) });
	 return std::nullopt;
 };

std::string BooleanLiteral::to_string() const {
	return value ? "true" : "false";
}

Object BooleanLiteral::eval(env_ptr env)  {
	return value;
}

std::optional<std::string> BooleanLiteral::compile(Compiler &compiler) {
	compiler.emit(value ? OpTrue : OpFalse);
	return std::nullopt;
}

std::string PrefixExpression::to_string() const {
	std::string result;
	result.reserve(64);
	result += "(";
	switch (type)
	{
	case token_type::Bang: result += '!'; break;
	case token_type::Dash: result += '-'; break;
	case token_type::Tilde: result += '~'; break;
	default: return "Error: Bad Token in PrefixExpresssion";
	}
	result += right->to_string();
	result += ")";
	return result;
}

Object PrefixExpression::eval(env_ptr env) {
	Object rv = right->eval(env);
	if (rv.isError()) return rv;
	switch (type)
	{
	case token_type::Bang: 
		return !rv.getBoolean();
	case token_type::Dash:
		if (!rv.is_a<std::int64_t>())
			return Object::make_error(std::format("unknown operator: -{}", rv.type_name()));

		return -rv.get<std::int64_t>();
	case token_type::Tilde:
		if (!rv.is_a<std::int64_t>())
			return Object::make_error(std::format("unknown operator: ~{}", rv.type_name()));

		return ~rv.get<std::int64_t>();
	}
	return Object::make_error("unexpected prefix operator");;
}

std::optional<std::string> PrefixExpression::compile(Compiler &compiler) {
	std::optional<std::string> result = right->compile(compiler);
	if (result)
		return result;

	switch (type)
	{
	case token_type::Bang:
		compiler.emit(OpBang);
		break;
	case token_type::Dash:
		compiler.emit(OpMinus);
		break;
	case token_type::Tilde:
		compiler.emit(OpTilde);
		break;
	}
	return std::nullopt;
}

const char *InfixExpression::operator_to_string() const {
	switch (type)
	{
	case token_type::Plus:		return "+";
	case token_type::Dash:		return "-";
	case token_type::Asterisk:	return "*";
	case token_type::ForwardSlash: return "/";
	case token_type::Ampersand: return "&";
	case token_type::Pipe:		return "|";
	case token_type::LogicAnd: return "&&";
	case token_type::LogicOr:		return "||";
	case token_type::LessThan:	return "<";
	case token_type::GreaterThan: return ">";
	case token_type::Equal:	return "==";
	case token_type::NotEqual:	return "!=";
	default: return "Error: Bad Token in InfixExpresssion";
	}
}

std::string InfixExpression::to_string() const {
	std::string result;
	result.reserve(64);
	result += "(" + left->to_string() + ' ' + operator_to_string() + ' ' + right->to_string() + ")";
	return result;
}

Object InfixExpression::eval(env_ptr env) {
	Object lv = left->eval(env);
	if (lv.isError()) return lv;

	if (lv.is_a<bool>())
	{
		switch (type)
		{
		case token_type::LogicAnd:
			if (!lv.get<bool>()) return false;
			else {
				Object rv = right->eval(env);
				if (rv.isError()) 
					return rv;
				else if (!rv.is_a<bool>()) 
					return Object::make_error(std::format("type mismatch: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));
				else return right->eval(env);
			}
		case token_type::LogicOr:
			if (lv.get<bool>()) return true;
			else {
				Object rv = right->eval(env);
				if (rv.isError()) 
					return rv;
				else if (!rv.is_a<bool>()) 
					return Object::make_error(std::format("type mismatch: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));
				else return right->eval(env);
			}
		}
	}

	Object rv = right->eval(env);
	if (rv.isError()) return rv;

	if (lv.index() != rv.index())
		return Object::make_error(std::format("type mismatch: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));
	switch (type)
	{
	case token_type::Hat:
		if (lv.is_a<bool>())
			return lv.get<bool>() ^ rv.get<bool>();;
		if (lv.is_a<std::int64_t>())
			return lv.get<std::int64_t>() ^ rv.get<std::int64_t>();
		else return Object::make_error(std::format("unknown operator: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));;
	case token_type::Plus:
		if (lv.is_a<std::string>()) // we already know rv is the same type
		{
			return lv.get<std::string>() + rv.get<std::string>();
		}
		[[fallthrough]];
	case token_type::Dash: 
	case token_type::Asterisk:
	case token_type::ForwardSlash:
	case token_type::Ampersand:
	case token_type::Pipe:
		if (!lv.is_a<std::int64_t>()) // we already know rv is the same type
			return Object::make_error(std::format("unknown operator: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));;
		switch (type)
		{
		case token_type::Plus:
			return lv.get<std::int64_t>() + rv.get<std::int64_t>();
		case token_type::Dash:
			return lv.get<std::int64_t>() - rv.get<std::int64_t>();
		case token_type::Asterisk:
			return lv.get<std::int64_t>() * rv.get<std::int64_t>();
		case token_type::ForwardSlash:
			return lv.get<std::int64_t>() / rv.get<std::int64_t>();
		case token_type::Ampersand:
			return lv.get<std::int64_t>() & rv.get<std::int64_t>();
		case token_type::Pipe:
			return lv.get<std::int64_t>() | rv.get<std::int64_t>();
		}
		break;
	case token_type::Equal:
		return lv == rv;
	case token_type::NotEqual:
		return lv != rv;
	case token_type::LessThan:
		return lv < rv;
	case token_type::GreaterThan:
		return lv > rv;
	}
	return Object::make_error("unexpected infix operator");
}

std::optional<std::string> InfixExpression::compile(Compiler &compiler) {
	std::optional<std::string> result;
	if (type == token_type::LogicAnd) {
		result = left->compile(compiler);
		if (result)
			return result;
		// If false, jump  to false result
		size_t jump_to_false1 = compiler.emit(OpJumpNotTruthy, { 9999 });
		result = right->compile(compiler);
		if (result)
			return result;
		// if false, jump to false result
		size_t jump_to_false2 = compiler.emit(OpJumpNotTruthy, { 9999 });
		// both true, push true result
		compiler.emit(OpTrue);
		size_t jump_after_true = compiler.emit(OpJump, { 9999 });
		size_t false_result = compiler.instructionSize();
		compiler.emit(OpFalse);
		size_t after_binary = compiler.instructionSize();
		compiler.changeOperand(jump_to_false1, { (std::int64_t)false_result });
		compiler.changeOperand(jump_to_false2, { (std::int64_t)false_result });
		compiler.changeOperand(jump_after_true, { (std::int64_t)after_binary });
		return std::nullopt;
	}
	else if (type == token_type::LogicOr) {
		result = left->compile(compiler);
		if (result)
			return result;
		// If true, jump  to true result
		size_t jump_to_true1 = compiler.emit(OpJumpTruthy, { 9999 });
		result = right->compile(compiler);
		if (result)
			return result;
		// if true, jump to true result
		size_t jump_to_true2 = compiler.emit(OpJumpTruthy, { 9999 });
		// both false, push false result
		compiler.emit(OpFalse);
		size_t jump_after_false = compiler.emit(OpJump, { 9999 });
		size_t true_result = compiler.instructionSize();
		compiler.emit(OpTrue);
		size_t after_binary = compiler.instructionSize();
		compiler.changeOperand(jump_to_true1, { (std::int64_t)true_result });
		compiler.changeOperand(jump_to_true2, { (std::int64_t)true_result });
		compiler.changeOperand(jump_after_false, { (std::int64_t)after_binary });
		return std::nullopt;
	}

	if (type == token_type::LessThan) {
		result = right->compile(compiler);
		if (result)
			return result;
		result = left->compile(compiler);
		if (result)
			return result;
	}
	else {
		result = left->compile(compiler);
		if (result)
			return result;
		result = right->compile(compiler);
		if (result)
			return result;
	}

	switch (type)
	{
	case token_type::Plus:
		compiler.emit(OpAdd);
		break;
	case token_type::Dash:
		compiler.emit(OpSub);
		break;
	case token_type::Asterisk:
		compiler.emit(OpMul);
		break;
	case token_type::ForwardSlash:
		compiler.emit(OpDiv);
		break;
	case token_type::Ampersand:
		compiler.emit(OpAnd);
		break;
	case token_type::Pipe:
		compiler.emit(OpOr);
		break;
	case token_type::Hat:
		compiler.emit(OpXor);
		break;
	case token_type::LessThan:
	case token_type::GreaterThan:
		compiler.emit(OpGreaterThan);
		break;
	case token_type::Equal:
		compiler.emit(OpEqual);
		break;
	case token_type::NotEqual:
		compiler.emit(OpNotEqual);
		break;
	default:
		return "Error: Bad Token in InfixExpresssion";
	}

	return std::nullopt;
}

std::string IfExpression::to_string() const {
	std::string result;
	result.reserve(128);
	result += "if " + condition->to_string() + ' ' + consequence->to_string();
	if (alternative)
		result += " else " + alternative->to_string();
	return result;
}

Object IfExpression::eval(env_ptr env) {
	Object cond = condition->eval(env);
	if (cond.isError()) return cond;
	if (cond.getBoolean()) return consequence->eval(env);
	else if (alternative) return alternative->eval(env);
	return Object();
}

std::optional<std::string> IfExpression::compile(Compiler &compiler) {
	std::optional<std::string> result = condition->compile(compiler);
	if (result)
		return result;

	size_t jump_false_pos = compiler.emit(OpJumpNotTruthy, { 9999 }); // Set correct value later

	result = consequence->compile(compiler);
	if (result)
		return result;
	if (compiler.lastInstructionIs(OpPop))
		compiler.removeLastInstruction();

	size_t jump_pos = compiler.emit(OpJump, { 9999 });
	size_t after_consequece_pos = compiler.instructionSize();
	compiler.changeOperand(jump_false_pos, { (std::int64_t)after_consequece_pos });

	if (!alternative) {
		compiler.emit(OpNull);
	}
	else {
		result = alternative->compile(compiler);
		if (result)
			return result;
		if (compiler.lastInstructionIs(OpPop))
			compiler.removeLastInstruction();
	}

	size_t after_alternative_pos = compiler.instructionSize();
	compiler.changeOperand(jump_pos, { (std::int64_t)after_alternative_pos });
	return std::nullopt;
}

std::string FunctionLiteral::to_string() const {
	std::string result;
	result.reserve(8 * parameters->size() + 128);
	if (name == "") result += "fn(";
	else result += name + '(';
	auto iter = parameters->begin();
	if (iter != parameters->end()) {
		result += *iter++;
		for (; iter != parameters->end(); ++iter)
			result += ", " + *iter;
	}
	result += ") " + body->to_string();
	return result;
}

Object FunctionLiteral::eval(env_ptr env) {
	func_ptr fn = make_function();
	fn->body = body;
	fn->parameters = parameters;
	if (!env->isEmpty())
		fn->env = std::make_shared<Environment>(*env);
	return Object(std::move(fn));
}

std::optional<std::string> FunctionLiteral::compile(Compiler &compiler) {
	Compiler inner(compiler);

	if (name != "")
		inner.defineFunction(name);
	for (const std::string &param : *parameters)
		inner.define(param);

	std::optional<std::string> error = body->compile(inner);
	if (error)
		return error;
	if (inner.lastInstructionIs(OpPop))
		inner.replaceLastPopWithReturn();
	if (!inner.lastInstructionIs(OpReturnValue))
		inner.emit(OpReturn);

	for (const Symbol &symbol : inner.get_free()) {
		load_symbol(symbol,compiler);
	}
	comp_func_ptr func = make_compiled_function(inner.extractInstructions(), inner.countLocals(), parameters->size());
	//compiler.emit(OpConstant, { (std::int64_t)compiler.add_constant(Object(func)) });
	compiler.emit(OpClosure, { (std::int64_t)compiler.add_constant(Object(func)), (std::int64_t)inner.get_free().size() });

	return std::nullopt;
};


std::string CallExpression::to_string() const {
	std::string result;
	result.reserve(64);
	result += function->to_string();
	result += '(';
	if (!arguments.empty()) {
		auto iter = arguments.begin();
		result += (*iter++)->to_string();
		for (; iter != arguments.end(); ++iter)
			result += ", " + (*iter)->to_string();
	}
	result += ')';
	return result;
}

Object CallExpression::eval(env_ptr env) {
	Object fn_obj = function->eval(env);
	if (fn_obj.isError())
		return fn_obj;
	if (fn_obj.is_a<func_ptr>())
	{
		const func_ptr &fn = fn_obj.get<func_ptr>();
		if (arguments.size() != fn->parameters->size())
			return Object::make_error("Wrong number of arguments for function called");

		env_ptr fn_env = std::make_shared<Environment>(fn->env);
		for (size_t i = 0; i < arguments.size(); ++i)
		{
			Object arg = arguments[i]->eval(env);
			if (arg.isError())
				return arg;
			fn_env->set((*fn->parameters)[i], arg);
		}
		Object result = fn->body->eval(fn_env);
		result.is_returned = false;
		return result;
	}
	if (fn_obj.is_a<builtin_ptr>())
	{
		std::vector<Object> args;
		args.reserve(arguments.size());
		for (size_t i = 0; i < arguments.size(); ++i)
		{
			Object arg = arguments[i]->eval(env);
			if (arg.isError())
				return arg;
			args.push_back(std::move(arg));
		}
		return fn_obj.get<builtin_ptr>()(args);
	}
	return Object::make_error("Expected a function to call!");
}

std::optional<std::string> CallExpression::compile(Compiler &compiler) {
	std::optional<std::string> error = function->compile(compiler);
	if (error) return error;
	for (const auto &argument : arguments)
		argument->compile(compiler);
	compiler.emit(OpCall, { (std::int64_t)arguments.size() });
	return std::nullopt;
}

std::string LetStatement::to_string() const {
	std::string result;
	result.reserve(64);
	result += "let " + name->to_string() + " = " + value->to_string() + ";\n";
	return result;
}

Object LetStatement::eval(env_ptr env) {
	Object val = value->eval(env);
	if (val.isError()) return val;
	env->set(name->name, val);
	return Object();
}

std::optional<std::string> LetStatement::compile(Compiler &compiler) {
	Symbol symbol = compiler.define(name->name);
	std::optional<std::string> error = value->compile(compiler);
	if (error) return error;

	if (symbol.scope == SymbolTable::GlobalScope)
		compiler.emit(OpSetGlobal, { symbol.index });
	else if (symbol.scope == SymbolTable::LocalScope)
		compiler.emit(OpSetLocal, { symbol.index });
	else return "unexpected scope type";
	return std::nullopt;
}
std::string ReturnStatement::to_string() const {
	std::string result;
	result.reserve(64);
	result += "return " + value->to_string() + ";\n";
	return result;
}

Object ReturnStatement::eval(env_ptr env) {
	Object result = value->eval(env);
	result.is_returned = true;
	return result;
}

std::optional<std::string> ReturnStatement::compile(Compiler &compiler) {
	std::optional<std::string> error = value->compile(compiler);
	if (error) return error;
	compiler.emit(OpReturnValue);
	return std::nullopt;
};

std::string ExpressionStatement::to_string() const {
	std::string result;
	result += value->to_string();
	return result;
}

Object ExpressionStatement::eval(env_ptr env) {
	return value->eval(env);
}

std::optional<std::string> ExpressionStatement::compile(Compiler &compiler) {
	std::optional<std::string> error = value->compile(compiler);
	if (error) return error;
	compiler.emit(OpPop);
	return std::nullopt;
}


std::string StringLiteral::to_string() const {
	return '"' + value + '"';
}

Object StringLiteral::eval(env_ptr env) {
	return value;
}

std::optional<std::string> StringLiteral::compile(Compiler &compiler) {
	size_t id = compiler.add_constant(Object(value));
	compiler.emit(OpConstant, { (std::int64_t)id });
	return std::nullopt;
}

std::string ArrayLiteral::to_string() const {
	std::string result;
	result.reserve(elements.size() * 32);
	auto iter = elements.begin();
	result += '[';
	if (iter != elements.end()) {
		result += (*iter++)->to_string();
		for (; iter != elements.end(); ++iter) {
			result += ", " + (*iter)->to_string();
		}
	}
	result += ']';
	return result;
}

Object ArrayLiteral::eval(env_ptr env) {
	auto result = make_array();
	for (auto &elem : elements)
	{
		Object o = elem->eval(env);
		if (o.isError())
			return o;
		result->push_back(std::move(o));
	}
	return result;
}

std::optional<std::string> ArrayLiteral::compile(Compiler &compiler) {
	std::optional<std::string> error;
	for (auto &elem : elements)
	{
		error = elem->compile(compiler);
		if (error) return error;
	}
	compiler.emit(OpArray, { (std::int64_t)elements.size() });
	return std::nullopt;
};

std::string IndexExpression::to_string() const {
	std::string result;
	result.reserve(32);
	result += '(' + left->to_string() + '[';
	result += index->to_string() + "])";
	return result;
}

Object IndexExpression::eval(env_ptr env) {
	Object lv = left->eval(env);
	if (lv.isError())
		return lv;
	Object iv = index->eval(env);
	if (iv.isError())
		return iv;

	if (lv.is_a<array_ptr>()) {
		if (!iv.is_a<std::int64_t>())
			return Object::make_error("Expected index into array to be an INTEGER");

		std::int64_t index_value = iv.get<std::int64_t>();
		if (index_value < 0 || index_value >= lv.get<array_ptr>()->size())
			return Object::make_error("Index into array is out of bounds");

		return (*lv.get<array_ptr>())[(size_t)index_value];
	}
	else if (lv.is_a<hashmap_ptr>()) {
		hashmap_ptr hash = lv.get<hashmap_ptr>();
		auto iter = hash->find(iv);
		if (iter == hash->end())
			return Object();
		return iter->second;
	}

	return Object::make_error("Index operator not supported: " + lv.type_name());
}

std::optional<std::string> IndexExpression::compile(Compiler &compiler) {
	std::optional<std::string> error;
	error = left->compile(compiler);
	if (error) return error;
	error = index->compile(compiler);
	if (error) return error;
	compiler.emit(OpIndex);
	return std::nullopt;
};

std::string HashLiteral::to_string() const {
	std::string result;
	result.reserve(64);
	result += '{';
	if (!elements.empty()) {
		auto iter = elements.begin();
		result += iter->first->to_string() + " : " + iter->second->to_string();
		++iter;
		for (; iter != elements.end(); ++iter)
			result += ", " + iter->first->to_string() + " : " + iter->second->to_string();
	}
	result += '}';
	return result;
}

Object HashLiteral::eval(env_ptr env) {
	auto hashlit = make_hash();

	for (auto &elem : elements) {
		auto key = elem.first->eval(env);
		if (key.isError())
			return key;

		auto value = elem.second->eval(env);
		if (value.isError())
			return value;

		hashlit->insert(std::make_pair(key, value));
	}
	return hashlit;
}

std::optional<std::string> HashLiteral::compile(Compiler &compiler) {
	std::optional<std::string> error;
	for (auto &elem : elements) {
		error = elem.first->compile(compiler);
		if (error) return error;
		error = elem.second->compile(compiler);
		if (error) return error;
	}

	compiler.emit(OpHash, { (std::int64_t)elements.size() * 2 });

	return std::nullopt;
};