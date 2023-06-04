#include <unistd.h>
#include <iostream>
#include <fstream>
#include <variant>

#include "lexer/lexer.hh"
#include "parser/program.hpp"
#include "parser/environment.hpp"


int main(int argc, char *const argv[])
{
	const auto global = std::make_shared<Environment>();
	std::vector<StatementP> statements;

	for(;;)
	{
		switch(getopt(argc, argv, "f:")) // note the colon (:) to indicate that 'b' has a parameter and is not a switch
		{
			case 'f':
			{
				std::ifstream ifs(optarg);
				std::string content{(std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>())};

				Lexer lexer{content};

				Value value;
				while (!lexer.eof()) {
					auto statement = Statement::parseStatement(lexer);
					if (statement) {
						value = statement->eval(global);
						statements.push_back(std::move(statement));
						lexer.get(TokenType::Semicolon);
					}
				}
				continue;
			}

			case '?':
			case 'h':
			default :
				printf("Help/Usage Example\n");
				break;

			case -1:
				break;
		}
		break;
	}

	//for (const auto& [key, value] : global->values)
	//	std::cout << "\t" << key << " = " << value << "\n";

	std::cout << "repl\n> ";

	for (std::string line; std::getline(std::cin, line); std::cout << "\n> ") {
		Lexer lexer{line};

		Value value;
		while (!lexer.eof()) {
			auto statement = Statement::parseStatement(lexer);
			if (statement) {
				value = statement->eval(global);
				statements.push_back(std::move(statement));
				lexer.get(TokenType::Semicolon);
			}
		}

		std::cout << value << "\n";
	}
}
