#include <iostream>
#include <variant>

#include "lexer/lexer.hh"
#include "parser/program.hpp"
#include "parser/environment.hpp"


int main() {

	std::cout << "repl\n> ";

	const auto global = std::make_shared<Environment>();

	for (std::string line; std::getline(std::cin, line); std::cout << "\n> ") {
		Lexer lexer{line};
		auto program = Program::parse(lexer);
		auto value = program->eval(global);
		std::cout << value << "\n";
	}
}
