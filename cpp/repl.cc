#include "repl.hh"
#include "lexer/lexer.hh"
#include "parser/parser.hh"

#include <iostream>

static const char *PROMPT = ">> ";

void do_repl(void)
{
	while (true) {
		std::cout << PROMPT;
		std::string line;
		std::getline(std::cin, line);
		lexer lex(std::move(line));
		Parser p(lex);

		std::unique_ptr<Program> program = p.parse();

		if (p.hasErrors()) {
			p.printErrors();
			continue;
		}
		
		std::cout << program->to_string() << '\n';

	}
}

int main()
{
	std::cout << "Hello! Welcome to the Monkey language!\n";
	do_repl();
	return 0;
}