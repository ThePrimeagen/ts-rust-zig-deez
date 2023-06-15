#include <catch2/catch_test_macros.hpp>
#include <parser/parser.hpp>

TEST_CASE("parse let declaration", "[constexpr][parser][let-stmt]") {
    using namespace mk;

    SECTION("integer literal") {
        constexpr auto source = CtString(R"(
            let five = 5;
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                DeclarationStmt<
                    Type<TypeKind::int_>,
                    IdentifierExpr<"five">,
                    Expr<IntegerLiteralExpr<5>>
                >
            >
        >);
    }
    
    SECTION("string literal") {
        constexpr auto source = CtString(R"(
            let five = "test deez nuts";
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                DeclarationStmt<
                    Type<TypeKind::int_>,
                    IdentifierExpr<"five">,
                    Expr<StringLiteralExpr<"test deez nuts">>
                >
            >
        >);
    }
    
    SECTION("array literal") {
        constexpr auto source = CtString(R"(
            let five = [1, 2, 3, 4, 5];
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                DeclarationStmt<
                    Type<TypeKind::int_>,
                    IdentifierExpr<"five">,
                    Expr<ArrayLiteralExpr<
                        Expr<IntegerLiteralExpr<1>>,
                        Expr<IntegerLiteralExpr<2>>,
                        Expr<IntegerLiteralExpr<3>>,
                        Expr<IntegerLiteralExpr<4>>,
                        Expr<IntegerLiteralExpr<5>>
                    >>
                >
            >
        >);
    }
    
    SECTION("hetrogenous literal") {
        constexpr auto source = CtString(R"(
            let five = [1, 2, "three", 4, "five"];
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                DeclarationStmt<
                    Type<TypeKind::int_>,
                    IdentifierExpr<"five">,
                    Expr<ArrayLiteralExpr<
                        Expr<IntegerLiteralExpr<1>>,
                        Expr<IntegerLiteralExpr<2>>,
                        Expr<StringLiteralExpr<"three">>,
                        Expr<IntegerLiteralExpr<4>>,
                        Expr<StringLiteralExpr<"five">>
                    >>
                >
            >
        >);
    }

}
