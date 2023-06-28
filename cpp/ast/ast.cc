#include "ast.hh"

std::string Program::to_string() {
	std::string result;
	result.reserve(1024); // just some value
	for (std::unique_ptr<Statement> &statement : statements) {
		result += statement->to_string();
	}
	return result;
}


std::string BlockStatement::to_string() {
	std::string result;
	result.reserve(1024); // just some value
	result += "{ ";
	for (std::unique_ptr<Statement> &statement : statements) {
		result += statement->to_string();
	}
	result += " }";
	return result;
}

std::string Identifier::to_string() {
	return name;
}

 std::string IntegerLiteral::to_string() {
	return std::to_string(value);
}

 std::string BooleanLiteral::to_string() {
	 return value ? "true" : "false";
 }

 std::string PrefixExpression::to_string() {
	 std::string result;
	 result.reserve(64);
	 result += "(";
	 switch (type)
	 {
	 case token_type::Bang: result += '!'; break;
	 case token_type::Dash: result += '-'; break;
	 default: return "Error: Bad Token in PrefixExpresssion";
	 }
	 result += right->to_string();
	 result += ")";
	 return result;
 }

 std::string InfixExpression::to_string() {
	 std::string result;
	 result.reserve(64);
	 result += "(";
	 result += left->to_string();
	 switch (type)
	 {
	 case token_type::Plus:		result += " + "; break;
	 case token_type::Dash:		result += " - "; break;
	 case token_type::Asterisk:	result += " * "; break;
	 case token_type::ForwardSlash: result += " / "; break;
	 case token_type::LessThan:	result += " < "; break;
	 case token_type::GreaterThan: result += " > "; break;
	 case token_type::Equal:		result += " == "; break;
	 case token_type::NotEqual:	result += " != "; break;
	 default: return "Error: Bad Token in PrefixExpresssion";
	 }
	 result += right->to_string();
	 result += ")";
	 return result;
 }

 std::string IfExpression::to_string() {
	 std::string result;
	 result.reserve(128);
	 result += "if ";
	 result += condition->to_string();
	 result += ' ';
	 result += consequence->to_string();
	 if (alternative) {
		 result += " else ";
		 result += alternative->to_string();
	 }
	 return result;
 }

 std::string FunctionLiteral::to_string() {
	 std::string result;
	 result.reserve(128);
	 result += "fn(";

	 auto iter = parameters.begin();

	 if (iter != parameters.end()) {
		 result += (*iter)->to_string();
		 ++iter;
		 for (; iter != parameters.end(); ++iter) {
			 result += ", ";
			 result += (*iter)->to_string();
		 }
	 }
	 result += ") ";
	 result += body->to_string();

	 return result;
 }

 std::string CallExpression::to_string() {
	 std::string result;
	 result.reserve(64);
	 result += function->to_string();
	 result += '(';
	 if (!arguments.empty()) {
		 auto iter = arguments.begin();
		 result += (*iter)->to_string();
		 ++iter;
		 for (; iter != arguments.end(); ++iter) {
			 result += ", ";
			 result += (*iter)->to_string();
		 }
	 }
	 result += ')';
	 return result;
 }

 std::string LetStatement::to_string() {
	 std::string result;
	 result.reserve(64);
	 result += "let ";
	 result += name->to_string();
	 result += " = ";
	 result += value->to_string();
	 result += ";\n";
	 return result;
 }

 std::string ReturnStatement::to_string() {
	 std::string result;
	 result.reserve(64);
	 result += "return ";
	 result += value->to_string();
	 result += ";\n";
	 return result;
 }

 std::string ExpressionStatement::to_string() {
	 std::string result;
	 result += value->to_string();
	 return result;
 }