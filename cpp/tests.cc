#include "tests.hh"

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

    test_lexer();
    test_to_string();
    std::cout << std::endl;
    test_parser();
    test_evaluation();

    return 0;
}
