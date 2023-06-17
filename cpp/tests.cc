#include <iostream>
#include <utility>

#include "lexer/lexer.hh"

#include <cstdint>

#if __cplusplus < 202302L
namespace std {
    std::uint32_t to_underlying(auto e) {
        return static_cast<unsigned>(e);
    }
}
#endif

void runTest(const std::string &name, lexer &lexer, std::vector<token> expected_tokens)
{
    token token;

    for (auto &expected : expected_tokens) {
        lexer.next_token(token);

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
                << expected.literal
                << " got "
                << token.literal
                << '\n';

            std::exit(1);
        }
    }

    std::cout << "Test '" << name << "' passed successfully\n";
}

void
test_empty_input(void)
{
    std::string input{ "" };

    std::vector<token> expected_tokens{
        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };

    runTest("Empty Tests", lex, expected_tokens);
    return;
}

void
test_simple_input(void)
{
    std::string input{ "=+(){},;" };

    std::vector<token> expected_tokens{
        {token_type::Assign, "="},
        {token_type::Plus, "+"},
        {token_type::LParen, "("},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::RSquirly, "}"},
        {token_type::Comma, ","},
        {token_type::Semicolon, ";"},
        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };

    runTest("Simple Tests", lex, expected_tokens);
    return;
}

void
test_complex_input(void)
{
    std::string input{ "let five = 5;\n"
        "    let ten = 10;\n"
        "    let add = fn(x, y) {\n"
        "        x + y;\n"
        "    };\n"
        "    let result = add(five, ten);\n"
        "!-/*5;\n"
        "5 < 10 > 5;\n"
        "if (5 < 10) {\n"
        "    return true;\n"
        "} else {\n"
        "    return false;\n"
        "}\n"
        "10 == 10;\n"
        "10 != 9;\n"
    };

    std::vector<token> expected_tokens{
        {token_type::Let, "let"},
        {token_type::Identifier, "five"},
        {token_type::Assign, "="},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "ten"},
        {token_type::Assign, "="},
        {token_type::Integer, "10"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "add"},
        {token_type::Assign, "="},
        {token_type::Function, "fn"},
        {token_type::LParen, "("},
        {token_type::Identifier, "x"},
        {token_type::Comma, ","},
        {token_type::Identifier, "y"},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::Identifier, "x"},
        {token_type::Plus, "+"},
        {token_type::Identifier, "y"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},
        {token_type::Semicolon, ";"},
        {token_type::Let, "let"},
        {token_type::Identifier, "result"},
        {token_type::Assign, "="},
        {token_type::Identifier, "add"},
        {token_type::LParen, "("},
        {token_type::Identifier, "five"},
        {token_type::Comma, ","},
        {token_type::Identifier, "ten"},
        {token_type::RParen, ")"},
        {token_type::Semicolon, ";"},
        {token_type::Bang, "!"},
        {token_type::Dash, "-"},
        {token_type::ForwardSlash, "/"},
        {token_type::Asterisk, "*"},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::Integer, "5"},
        {token_type::LessThan, "<"},
        {token_type::Integer, "10"},
        {token_type::GreaterThan, ">"},
        {token_type::Integer, "5"},
        {token_type::Semicolon, ";"},
        {token_type::If, "if"},
        {token_type::LParen, "("},
        {token_type::Integer, "5"},
        {token_type::LessThan, "<"},
        {token_type::Integer, "10"},
        {token_type::RParen, ")"},
        {token_type::LSquirly, "{"},
        {token_type::Return, "return"},
        {token_type::True, "true"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},
        {token_type::Else, "else"},
        {token_type::LSquirly, "{"},
        {token_type::Return, "return"},
        {token_type::False, "false"},
        {token_type::Semicolon, ";"},
        {token_type::RSquirly, "}"},

        {token_type::Integer, "10"},
        {token_type::Equal, "=="},
        {token_type::Integer, "10"},
        {token_type::Semicolon, ";"},

        {token_type::Integer, "10"},
        {token_type::NotEqual, "!="},
        {token_type::Integer, "9"},
        {token_type::Semicolon, ";"},

        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };
    runTest("Complex Tests", lex, expected_tokens);

    return;
}

int
main(void)
{
    test_empty_input();
    test_simple_input();
    test_complex_input();

    return 0;
}
