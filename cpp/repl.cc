#include "repl.hh"
#include "lexer/lexer.hh"
#include "parser/parser.hh"

#include <iostream>

static const char *PROMPT = ">> ";

void do_repl(void)
{
	env_ptr env = std::make_shared<Environment>();
	while (true) {
		std::cout << PROMPT;
		std::string line;
		std::getline(std::cin, line);
		lexer lex(std::move(line));
		Parser p(lex);

		auto program = p.parse();

		if (p.hasErrors()) {
			p.printErrors();
			continue;
		}

		Object value = program->eval(env);
		std::cout << value.to_string() << std::endl;

	}
}

int main()
{
	std::cout << "Hello! Welcome to the Monkey language!\n";
	do_repl();
	return 0;
}