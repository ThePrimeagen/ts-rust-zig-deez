#include "tests.hh"
#include "vm/vm.hh"

#ifdef WIN32
#include <windows.h>
#include <vld.h>
#endif

void test_to_string() {
    TestHelper tt("to_string method");
   
    std::unique_ptr<LetStatement> let = std::make_unique<LetStatement>();
    let->name = std::make_unique<Identifier>("myVar");
    let->value = std::make_unique<Identifier>("anotherVar");
   
    auto program = std::make_shared<Program>();
    program->statements.push_back(std::move(let));

    auto result = program->to_string();
    
    if (result != "let myVar = anotherVar;\n") {
        tt.fail("Unexpected result from to_string(): ", result);
        return;
    }

}

void benchmark() {

    std::string input(
        "let fibonacci = fn(x) {\n"
        "   if (x == 0) {\n"
        "       0\n"
        "   }\n"
        "   else {\n"
        "       if (x == 1) {\n"
        "           return 1;\n"
        "       }\n"
        "       else {\n"
        "           fibonacci(x - 1) + fibonacci(x - 2);\n"
        "       }\n"
        "   }\n"
        "};\n"
        "fibonacci(35);\n");
    auto program = parse(std::move(input));
    GlobalData globals;
    Compiler compiler(&globals);
    std::optional<std::string> error = program->compile(compiler);
    if (error) {
        std::cerr << "Fatal - compiler Error: " << *error << std::endl;
        return;
    }

    Environment env;
    std::chrono::time_point<std::chrono::steady_clock> start_eval{ std::chrono::steady_clock::now() };
    Object result_eval = program->eval(&env);
    std::chrono::time_point<std::chrono::steady_clock> end_eval{ std::chrono::steady_clock::now() };
    std::cout << "Eval: result is " << result_eval.to_string()
        << " duration " << std::chrono::duration_cast<std::chrono::seconds>(end_eval - start_eval)
        << std::endl;

    SwitchVM vm(&globals);
    Bytecode bytecode(std::move(compiler));
    std::chrono::time_point<std::chrono::steady_clock> start_vm{std::chrono::steady_clock::now()};
    error = vm.run(std::move(bytecode));
    if (error) {
        std::cerr << "Error executing code: " << *error << std::endl;
        return;
    }
    std::chrono::time_point<std::chrono::steady_clock> end_vm{ std::chrono::steady_clock::now() };
    Object result_vm = vm.last_popped_element();
    std::cout << "VM: result is " << result_vm.to_string()
        << " duration " << std::chrono::duration_cast<std::chrono::seconds>(end_vm - start_vm)
        << std::endl;
}

void print_sizes() {
    // std::monostate, Error, bool, std::int64_t, std::string, func_ptr, hashmap_ptr, builtin_ptr, array_ptr, comp_func_ptr, closure_ptr 
    std::cout
        << "Object: " << sizeof(Object) << '\n'
        << "std::monostate: " << sizeof(std::monostate) << '\n'
        << "bool: " << sizeof(bool) << '\n'
        << "std::int64_t: " << sizeof(std::int64_t) << '\n'
        << "std::string: " << sizeof(std::string) << '\n'
        << "func_ptr: " << sizeof(func_ptr) << '\n'
        << "hashmap_ptr: " << sizeof(hashmap_ptr) << '\n'
        << "builtin_ptr: " << sizeof(builtin_ptr) << '\n'
        << "array_ptr: " << sizeof(array_ptr) << '\n'
        << "comp_func_ptr: " << sizeof(comp_func_ptr) << '\n'
        << "closure_ptr: " << sizeof(closure_ptr) << '\n'
        ;
}

int
main(void)
{

#ifdef WIN32
    // Set output mode to handle virtual terminal sequences, i.e. fancy colors
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE)
    {
        return GetLastError();
    }

    DWORD dwMode = 0;
    if (!GetConsoleMode(hOut, &dwMode))
    {
        return GetLastError();
    }

    dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if (!SetConsoleMode(hOut, dwMode))
    {
        return GetLastError();
    }
#endif
    print_sizes();

    test_lexer();
    test_to_string();
    std::cout << std::endl;
    test_parser();
    test_evaluation();
    test_symbol_table();
    test_code();
    test_vm();

    //benchmark();
    return 0;
}
