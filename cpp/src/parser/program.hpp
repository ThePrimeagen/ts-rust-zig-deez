#pragma once

#include <vector>

#include "statement.hpp"

struct Program;
using ProgramP = std::unique_ptr<Program>;

class Lexer;
struct Program : Expression
{
	static ProgramP parse(Lexer& lexer);
	void add(Lexer& lexer);

	Value run();

	Value eval(EnvironmentP env) const override;
	void print(std::ostream& str) const override;

	std::vector<StatementP> statements;
};

std::ostream& operator<<(std::ostream& os, const Program& program);
