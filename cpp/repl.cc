#include "lexer/lexer.hh"
#include "parser/parser.hh"
#include "vm/vm.hh"

#include <iostream>
#include <fstream>
#include <filesystem>


static const char *PROMPT = ">> ";

void do_file(std::string filename, array_ptr &&args, bool use_vm = false)
{
	std::string script;
	if (std::ifstream is{ filename, std::ios::binary | std::ios::ate }) {
		auto size = is.tellg();
		script.resize(size, '\0'); // construct string to stream size
		is.seekg(0);
		if (is.read(&script[0], size))
			return;
	}
	else {
		std::cerr << "Failed to open " << filename << "!!!" << std::endl;
		exit(1);
	}
	lexer lex(std::move(script));
	Parser p(lex);

	auto program = p.parse();

	if (p.hasErrors()) {
		p.printErrors();
		exit(1);
	}

	Object value;
	if (use_vm) {
		GlobalData globals;
		Symbol sym = globals.symbolTable.define("args");
		globals.globals[sym.index] = args;
		SwitchVM vm(&globals);
		Compiler compiler(&globals);
		std::optional<std::string> error = program->compile(compiler);
		if (error) {
			std::cerr << "Error compiling to bytecode: " << *error << std::endl;
			return;
		}
		Bytecode bytecode(std::move(compiler));
		error = vm.run(std::move(bytecode));
		if (error) {
			std::cerr << "Error executing bytecode: " << *error << std::endl;
			return;
		}
		value = vm.last_popped_element();
	}
	else {
		Environment env;
		env.set("args", args);
		value = program->eval(&env);
	}
}

void do_repl(array_ptr &&args, bool use_vm=false)
{
	Environment env;
	GlobalData globals;
	SwitchVM vm(&globals);
	if (!use_vm) {
		env.set("args", args);
	}
	else {
		Symbol sym = globals.symbolTable.define("args");
		globals.globals[sym.index] = args;
	}
	Object value;
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

		if (use_vm) {
			Compiler compiler(&globals);
			std::optional<std::string> error = program->compile(compiler);
			if (error) {
				std::cerr << "Error compiling to bytecode: " << *error << std::endl;
				continue;
			}
			Bytecode bytecode(std::move(compiler));
			error = vm.run(std::move(bytecode));
			if (error) {
				std::cerr << "Error executing bytecode: " << *error << std::endl;
				continue;
			}
			value = vm.last_popped_element();
		}
		else {
			value = program->eval(&env);
		}
		std::cout << value.to_string() << std::endl;
	}
}

void printHelp(const char *name) {
	std::filesystem::path path(name);
	std::cout << "Usage: " << path.filename().string() << " [args] [script [scriptargs]]\n"
		<< "Where args may be\n"
		<< "\t-h: print this message then exit.\n"
		<< "\t-ast: execute code using ast traversal\n"
		<< "\t or\n" 
		<< "\t-vm (default): execute code by compiling to bytecode, then execution with SwitchVM.\n"
		<< "- script may be a file name with  a script to execute, or -- for REPL from stdin.\n"
		<< "- scriptargs: Any parameters after script are written to array 'args' accessible by code executed.\n"
		<< std::endl;
}

int main(int argc, const char *argv[])
{
	bool use_vm = true;
	std::string script("--");
	array_ptr args = make_array();
	int state = 0;
	for (size_t i = 1; i < argc; ++i) {
		std::string arg = argv[i]; // simplify comparison
		if (state == 0)
		{
			if (arg.front() != '-' || arg == "--") {
				script = arg;
				state = 1;
			}
			else if (arg == "-h") {
				printHelp(argv[0]);
				return 0;
			}
			else if (arg == "-vm") use_vm = true;
			else if (arg == "-ast") use_vm = false;
		}
		else args->push_back(arg);
	}

	std::cout << "Hello! Welcome to the Monkey language!\n";
	if (script == "--")
		do_repl(std::move(args), use_vm);
	else
		do_file(script, std::move(args), use_vm);
	return 0;
}