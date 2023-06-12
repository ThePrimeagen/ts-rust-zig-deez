#include <unistd.h>
#include <iostream>
#include <fstream>
#include <variant>

#include "lexer/lexer.hh"
#include "parser/program.hpp"
#include "parser/environment.hpp"
#include "parser/builtins.hpp"

int main(int argc, char *const argv[])
{
	const auto global = std::make_shared<Environment>();
	for (const auto& builtin : BuiltinFunctionExpression::builtins)
		global->set(builtin.name, Value{BoundFunction{&builtin, {}}});
	for (const auto& [token, builtin] : BuiltinBinaryFunctionExpression::builtins)
		global->set(builtin->name, Value{BoundFunction{builtin, {}}});

	std::vector<ExpressionP> statements;
	std::vector<Lexer> lexers;

	for(;;)
	{
		switch(getopt(argc, argv, "f:")) // note the colon (:) to indicate that 'b' has a parameter and is not a switch
		{
			case 'f':
			{
				std::ifstream ifs(optarg);
				if (!ifs) {
					std::cerr << "could not find file: " << optarg << "\n";
					exit(1);
				}
				std::string content{(std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>())};

				Lexer lexer{content};
				auto statementList = StatementList::parse(lexer);
				statementList->eval(global);
				statements.push_back(std::move(statementList));
				lexers.push_back(std::move(lexer));
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

	std::cout << "repl\n> ";

	for (std::string line; std::getline(std::cin, line); std::cout << "\n> ") {
		Lexer lexer{line};

		Value value;
		while (!lexer.eof()) {
			try {
				if (auto statement = Statement::parseStatement(lexer); statement) {
					value = statement->eval(global);
					statements.push_back(std::move(statement));
					lexer.get(TokenType::Semicolon);
				}
			}
			catch (const std::exception& ex) {
				std::cout << "error: " << ex.what() << "\n";
				if (!lexer.eof())
					lexer.next();
			}
		}

		lexers.push_back(std::move(lexer));

		std::cout << value << "\n";
	}
}
