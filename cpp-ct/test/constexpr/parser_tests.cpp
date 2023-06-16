#include <catch2/catch_test_macros.hpp>
#include <parser/parser.hpp>

using namespace mk;

TEST_CASE("parse let declaration", "[constexpr][parser][let-stmt]") {

    SECTION("integer literal") {
        constexpr auto source = CtString(R"(
            let five = 5;
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                BlockStmt<
                    DeclarationStmt<
                        Type<TypeKind::int_>,
                        IdentifierExpr<"five">,
                        Expr<IntegerLiteralExpr<5>>
                    >
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
                BlockStmt<
                    DeclarationStmt<
                        Type<TypeKind::int_>,
                        IdentifierExpr<"five">,
                        Expr<StringLiteralExpr<"test deez nuts">>
                    >
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
                BlockStmt<
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
            >
        >);
    }
    
    SECTION("heterogenous literal") {
        constexpr auto source = CtString(R"(
            let five = [1, 2, "three", 4, "five"];
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        static_assert(std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                BlockStmt<
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
            >
        >);
    }
}

TEST_CASE("parse if statement", "[constexpr][parser][if-stmt]") {

    SECTION("single if stmt") {
        constexpr auto source = CtString(R"(
            if (true) {
                5
            }
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();
        
        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        IfStmt<
                            Expr<BoolLiteralExpr<true>>,
                            BlockStmt<
                                Expr<IntegerLiteralExpr<5>>
                            >,
                            void
                        >
                    >
                >
            >
        );
    }

    SECTION("two level of condition") {
        constexpr auto source = CtString(R"(
            if (true) {
                5
            } else {
                6
            }
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        IfStmt<
                            Expr<BoolLiteralExpr<true>>,
                            BlockStmt<
                                Expr<IntegerLiteralExpr<5>>
                            >,
                            BlockStmt<
                                Expr<IntegerLiteralExpr<6>>
                            >
                        >
                    >
                >
            >
        );
    }

    SECTION("three level of condition") {
        constexpr auto source = CtString(R"(
            if (true) {
                5
            } else if (false) {
                6
            } else {
                7
            }
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        IfStmt<
                            Expr<BoolLiteralExpr<true>>,
                            BlockStmt<
                                Expr<IntegerLiteralExpr<5>>
                            >,
                            IfStmt<
                                Expr<BoolLiteralExpr<false>>,
                                BlockStmt<
                                    Expr<IntegerLiteralExpr<6>>
                                >,
                                BlockStmt<
                                    Expr<IntegerLiteralExpr<7>>
                                >
                            >
                        >
                    >
                >
            >
        );
    }
}
