#include <catch2/catch_test_macros.hpp>
#include <utils/ct_string.hpp>

TEST_CASE("compile-time string construction", "[constexpr][ct-string][construction]") {
    constexpr auto str = mk::CtString("Hello, World!");
    static_assert(str.size() == 13);
}

TEST_CASE("compile-time string equality tests", "[constexpr][ct-string][equality]") {
    constexpr auto str = mk::CtString("Hello, World!");
    static_assert(str.size() == 13);
    static_assert(str == mk::CtString("Hello, World!"));
    static_assert(str == "Hello, World!");
    static_assert(str == std::string_view("Hello, World!"));
    REQUIRE((str == std::string("Hello, World!")));
    
    static_assert(str != mk::CtString("Hello"));
    static_assert(str != "Hello");
    static_assert(str != std::string_view("Hello"));
    REQUIRE((str != std::string("Hello")));
}
