#include "program.hpp"

#include "lexer.hh"
#include "statement.hpp"


ProgramP Program::parse(Lexer& lexer)
{
	auto program = std::make_unique<Program>();
	program->add(lexer);
	return program;
}

void Program::add(Lexer& lexer)
{
	while (!lexer.eof()) {
		auto statement = Statement::parseStatement(lexer);
		if (statement) {
			statements.push_back(std::move(statement));
			lexer.get(TokenType::Semicolon);
		}
	}
}

Value Program::run()
{
	const auto global = std::make_shared<Environment>();
	return eval(global);
}

Value Program::eval(EnvironmentP env) const
{
	try {
		Value value;
		for (const auto& statement : statements) {
			value = statement->eval(env);
		}
		return value;
	}
	catch (const Value& value) {
		return value;
	}
}

void Program::print(std::ostream& os) const
{
	for (const auto& statement : statements)
		os << *statement << ";\n";
}

std::ostream& operator<<(std::ostream& os, const Program& program)
{
	for (const auto& statement : program.statements) {
		os << *statement << "\n";
	}
	return os;
}
