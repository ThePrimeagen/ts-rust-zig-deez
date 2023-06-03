#include <iostream>

#include "lexer/lexer.hh"

int main() {

	std::cout << "repl\n> ";

	for (std::string line; std::getline(std::cin, line); std::cout << "\n> ")
		for (Lexer lexer{line}; !lexer.eof(); lexer.next())
			std::cout << lexer.type() << " " << lexer.literal() << "\n"; 


}
