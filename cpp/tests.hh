#pragma once

#include <iostream>
#include <utility>
#include <cstdint>
#include <chrono>

#include "lexer/lexer.hh"
#include "ast/ast.hh"
#include "parser/parser.hh"

class TestHelper {

    const char *RESET = "\033[0m";
    const char *BLACK = "\033[30m";
    const char *RED = "\033[31m";
    const char *GREEN = "\033[32m";
    const char *YELLOW = "\033[33m";
    const char *BLUE = "\033[34m";
    const char *PURPLE = "\033[35m";
    const char *CYAN = "\033[36m";
    const char *WHITE = "\033[37m";
    std::string_view name;
    std::chrono::time_point<std::chrono::steady_clock> start;

    bool failed = false;
public:

    explicit TestHelper(const std::string_view n) : name(n), start(std::chrono::steady_clock::now())
    {
        std::cout << ">> Test '" << name << "' begins\n";
    };

    ~TestHelper()
    {
        std::chrono::time_point<std::chrono::steady_clock> end = std::chrono::steady_clock::now();
        std::cout << "<< ";
        if (failed) std::cout << RED << "FAILED" << RESET;
        else std::cout << GREEN << "PASSED" << RESET;
        std::cout << " test '" << name << "' - duration: "
            << std::chrono::duration_cast<std::chrono::microseconds>(end - start) << std::endl;
    }

    void fail() { std::cerr << std::endl; failed = true; }

    template<typename... Targs>
    void fail(Object value, Targs... Fargs)
    {
        std::cerr << value.to_string();
        fail(Fargs...);
    }

    template<typename T, typename... Targs>
    void fail(T value, Targs... Fargs)
    {
        std::cerr << value;
        fail(Fargs...);
    }
};

inline std::shared_ptr<Program> parse(std::string &&input) {
    lexer lex(std::move(input));
    Parser p(lex);
    return p.parse();
}

void test_lexer();
void test_parser();
void test_evaluation();
void test_symbol_table();
void test_code();
void test_vm();
