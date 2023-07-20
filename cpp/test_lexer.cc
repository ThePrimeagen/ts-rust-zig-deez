#include "tests.hh"

void runLexerTest(const std::string &name, lexer &lexer, std::vector<token> expected_tokens)
{
    token token;
    TestHelper tt(name);
    for (auto &expected : expected_tokens) {
        lexer.next_token(token);

        if (token.type != expected.type) {
            tt.fail("Expected token type: "
                , getTokenTypeName(expected.type)
                , " got "
                , getTokenTypeName(token.type));

            return;
        }
        else if (token.literal != expected.literal) {
            tt.fail("Expected token literal: "
                , expected.literal
                , " got "
                , token.literal);

            return;
        }
    }
}

void
test_empty_input(void)
{
    std::string input{ "" };

    std::vector<token> expected_tokens{
        {token_type::Eof, "\0"}
    };

    lexer lex{ std::move(input) };

    runLexerTest("Empty Tests", lex, expected_tokens);
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

    runLexerTest("Simple Tests", lex, expected_tokens);
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
        "\"foobar\"\n"
        "\"foo bar\"\n"
        "[0, 1];\n"
        "{\"foo\":\"bar\"}\n"
        "~1|2&false\n"
        "1||2&&3\n"
        "1.1 .22e-18"
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
        {token_type::String, "foobar"},
        {token_type::String, "foo bar"},
        {token_type::LBracket, "["},
        {token_type::Integer, "0"},
        {token_type::Comma, ","},
        {token_type::Integer, "1"},
        {token_type::RBracket, "]"},
        {token_type::Semicolon, ";"},
        {token_type::LSquirly, "{"},
        {token_type::String, "foo"},
        {token_type::Colon, ":"},
        {token_type::String, "bar"},
        {token_type::RSquirly, "}"},
        
        {token_type::Tilde, "~"},
        {token_type::Integer, "1"},
        {token_type::Pipe, "|"},
        {token_type::Integer, "2"},
        {token_type::Ampersand, "&"},
        {token_type::False, "false"},
        
        {token_type::Integer, "1"},
        {token_type::LogicOr, "||"},
        {token_type::Integer, "2"},
        {token_type::LogicAnd, "&&"},
        {token_type::Integer, "3"},
        // 1.1 .22e-18
        {token_type::FloatingPoint, "1.1"},
        {token_type::FloatingPoint, ".22e-18"},
        {token_type::Eof, "\0"},

    };

    lexer lex{ std::move(input) };
    runLexerTest("Complex Tests", lex, expected_tokens);

    return;
}

void test_lexer()
{
    std::cout << ">>> Lexer tests <<<\n";
    test_empty_input();
    test_simple_input();
    test_complex_input();
    std::cout << std::endl;
}
