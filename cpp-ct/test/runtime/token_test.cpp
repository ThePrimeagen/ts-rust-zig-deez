#include <catch2/catch_test_macros.hpp>
#include <syntax/tokens.hpp>

TEST_CASE("TokenKind", "[runtime][token]") {
    SECTION("TokenKind::to_string") {
        REQUIRE((mk::to_string(mk::TokenKind::identifier) == "identifier"));
        REQUIRE((mk::to_string(mk::TokenKind::int_literal) == "int_literal"));
        REQUIRE((mk::to_string(mk::TokenKind::string_literal) == "string_literal"));
        REQUIRE((mk::to_string(mk::TokenKind::bool_literal) == "bool_literal"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_int) == "kw_int"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_fn) == "kw_fn"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_let) == "kw_let"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_if) == "kw_if"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_else) == "kw_else"));
        REQUIRE((mk::to_string(mk::TokenKind::kw_return) == "kw_return"));
        REQUIRE((mk::to_string(mk::TokenKind::open_paren) == "open_paren"));
        REQUIRE((mk::to_string(mk::TokenKind::close_paren) == "close_paren"));
    }
}