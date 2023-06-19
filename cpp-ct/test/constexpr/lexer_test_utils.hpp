#if !defined(MK_TEST_CONSTEXPR_LEXER_TEST_UTILS_HPP)
#define MK_TEST_CONSTEXPR_LEXER_TEST_UTILS_HPP

#include <utils/ct_string.hpp>
#include <syntax/tokens.hpp>
#include <array>

namespace mk::test  {
    
    struct TestInput1 {
        static constexpr auto source = mk::CtString(R"(
            let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                    x + y;
                };
                let result = add(five, ten);
            !-/ *5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        )");

        static constexpr std::array tokens = {
            Token(TokenKind::kw_let, "let"),
            Token(TokenKind::identifier, "five"),
            Token(TokenKind::equal, "="),
            Token(TokenKind::int_literal, "5"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::kw_let, "let"),
            Token(TokenKind::identifier, "ten"),
            Token(TokenKind::equal, "="),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::kw_let, "let"),
            Token(TokenKind::identifier, "add"),
            Token(TokenKind::equal, "="),
            Token(TokenKind::kw_fn, "fn"),
            Token(TokenKind::open_paren, "("),
            Token(TokenKind::identifier, "x"),
            Token(TokenKind::comma, ","),
            Token(TokenKind::identifier, "y"),
            Token(TokenKind::close_paren, ")"),
            Token(TokenKind::open_brace, "{"),
            Token(TokenKind::identifier, "x"),
            Token(TokenKind::plus, "+"),
            Token(TokenKind::identifier, "y"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::close_brace, "}"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::kw_let, "let"),
            Token(TokenKind::identifier, "result"),
            Token(TokenKind::equal, "="),
            Token(TokenKind::identifier, "add"),
            Token(TokenKind::open_paren, "("),
            Token(TokenKind::identifier, "five"),
            Token(TokenKind::comma, ","),
            Token(TokenKind::identifier, "ten"),
            Token(TokenKind::close_paren, ")"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::exclamation_mark, "!"),
            Token(TokenKind::minus, "-"),
            Token(TokenKind::slash, "/"),
            Token(TokenKind::star, "*"),
            Token(TokenKind::int_literal, "5"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::int_literal, "5"),
            Token(TokenKind::less_than, "<"),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::greater_than, ">"),
            Token(TokenKind::int_literal, "5"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::kw_if, "if"),
            Token(TokenKind::open_paren, "("),
            Token(TokenKind::int_literal, "5"),
            Token(TokenKind::less_than, "<"),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::close_paren, ")"),
            Token(TokenKind::open_brace, "{"),
            Token(TokenKind::kw_return, "return"),
            Token(TokenKind::bool_literal, "true"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::close_brace, "}"),
            Token(TokenKind::kw_else, "else"),
            Token(TokenKind::open_brace, "{"),
            Token(TokenKind::kw_return, "return"),
            Token(TokenKind::bool_literal, "false"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::close_brace, "}"),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::equal_equal, "=="),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::int_literal, "10"),
            Token(TokenKind::not_equal, "!="),
            Token(TokenKind::int_literal, "9"),
            Token(TokenKind::semicolon, ";"),
            Token(TokenKind::eof, "EOF")
        };
    };

} // namespace mk


#endif // MK_TEST_CONSTEXPR_LEXER_TEST_UTILS_HPP
