#include "ast.hh"

extern std::unordered_map<std::string, builtin_ptr> builtins;

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

size_t ObjectHasher::operator()(const Object & obj) const {
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
	for (std::initializer_list<Object>::iterator ait = args.begin(); ait != args.end(); ++ait, ++pit)
		fn_env->set(*pit, *ait);
	return 	body->eval(fn_env);
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

 std::string IntegerLiteral::to_string() const {
	return std::to_string(value);
}

 Object IntegerLiteral::eval(env_ptr env)  {
	 return value;
 }

std::string BooleanLiteral::to_string() const {
	return value ? "true" : "false";
}

Object BooleanLiteral::eval(env_ptr env)  {
	return value;
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
	case token_type::Bang: {
		auto b = rv.getBoolean();
		if (b) return !*b;
		else return Object::make_error("Logical not requires boolean operand");
	}
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

const char *InfixExpression::operator_to_string() const {
	switch (type)
	{
	case token_type::Plus:		return "+";
	case token_type::Dash:		return "-";
	case token_type::Asterisk:	return "*";
	case token_type::ForwardSlash: return "/";
	case token_type::Ampersand: return "&";
	case token_type::Pipe:		return "|";
	case token_type::LessThan:	return "<";
	case token_type::GreaterThan: return ">";
	case token_type::Equal:	return "==";
	case token_type::NotEqual:	return "!=";
	default: return "Error: Bad Token in PrefixExpresssion";
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
		case token_type::Ampersand:
			if (!lv.get<bool>()) return false;
			else {
				Object rv = right->eval(env);
				if (rv.isError()) 
					return rv;
				else if (!rv.is_a<bool>()) 
					return Object::make_error(std::format("type mismatch: {} {} {}", lv.type_name(), operator_to_string(), rv.type_name()));
				else return right->eval(env);
			}
		case token_type::Pipe:
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
	auto condval = cond.getBoolean();
	if (!condval) return Object::make_error("Condition in IF not truthlike");
	if (*condval) return consequence->eval(env);
	else if (alternative) return alternative->eval(env);
	return Object();
}

std::string FunctionLiteral::to_string() const {
	std::string result;
	result.reserve(8 * parameters->size() + 128);
	result += "fn(";

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

std::string ExpressionStatement::to_string() const {
	std::string result;
	result += value->to_string();
	return result;
}

Object ExpressionStatement::eval(env_ptr env) {
	return value->eval(env);
}

std::string StringLiteral::to_string() const {
	return '"' + value + '"';
}

Object StringLiteral::eval(env_ptr env) {
	return value;
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

		std::int64_t index = iv.get<std::int64_t>();
		if (index < 0 || index >= lv.get<array_ptr>()->size())
			return Object::make_error("Index into array is out of bounds");

		return (*lv.get<array_ptr>())[(size_t)index];
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