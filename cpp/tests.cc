#include <iostream>
#include <utility>

#include "lexer/lexer.hh"


void
test_simple_input(void)
{
    std::string input{"=+(){},;"};

    token expected_tokens[9] = {
        {token_type::assign, '='},
        {token_type::plus, '+'},
        {token_type::lparen, '('},
        {token_type::rparen, ')'},
        {token_type::lsquirly, '{'},
        {token_type::rsquirly, '}'},
        {token_type::comma, ','},
        {token_type::semicolon, ';'},
        {token_type::eof, '\0'},
    };

    lexer lex{input};
    token token;

    for (auto &expected : expected_tokens) {
        lex.next_token(token);

        if (token.type != expected.type) {
            std::cerr
                << "Expected token type: "
                << std::to_underlying(expected.type)
                << " got "
                << std::to_underlying(token.type)
                << '\n';

            std::exit(1);
        }
        else if (token.literal != expected.literal) {
            std::cerr
                << "Expected token literal: "
                << std::get<char>(expected.literal)
                << " got "
                << std::get<char>(token.literal)
                << '\n';

            std::exit(1);
        }
    }

    std::cout << "All tests passed successfully\n";

    return;
}


int
main(void)
{
    test_simple_input();

    return 0;
}
