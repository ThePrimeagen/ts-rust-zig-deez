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
                        IntegerLiteralExpr<5>
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
                        StringLiteralExpr<"test deez nuts">
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
                        ArrayLiteralExpr<
                            IntegerLiteralExpr<1>,
                            IntegerLiteralExpr<2>,
                            IntegerLiteralExpr<3>,
                            IntegerLiteralExpr<4>,
                            IntegerLiteralExpr<5>
                        >
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
                        ArrayLiteralExpr<
                            IntegerLiteralExpr<1>,
                            IntegerLiteralExpr<2>,
                            StringLiteralExpr<"three">,
                            IntegerLiteralExpr<4>,
                            StringLiteralExpr<"five">
                        >
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
                            BoolLiteralExpr<true>,
                            BlockStmt<
                                IntegerLiteralExpr<5>
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
                            BoolLiteralExpr<true>,
                            BlockStmt<
                                IntegerLiteralExpr<5>
                            >,
                            BlockStmt<
                                IntegerLiteralExpr<6>
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
                            BoolLiteralExpr<true>,
                            BlockStmt<
                                IntegerLiteralExpr<5>
                            >,
                            IfStmt<
                                BoolLiteralExpr<false>,
                                BlockStmt<
                                    IntegerLiteralExpr<6>
                                >,
                                BlockStmt<
                                    IntegerLiteralExpr<7>
                                >
                            >
                        >
                    >
                >
            >
        );
    }
}

TEST_CASE("parse while statement", "[constexpr][parser][while-stmt]") {
    constexpr auto source = CtString(R"(
        while(true) {
            5
        }
    )");
    constexpr auto ast = parse_tokens<Lexer<source>>();

    static_assert(
        std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                BlockStmt<
                    WhileStmt<
                        BoolLiteralExpr<true>,
                        BlockStmt<
                            IntegerLiteralExpr<5>
                        >
                    >
                >
            >
        >
    );
}

TEST_CASE("parse return statement", "[constexpr][parser][return-stmt]") {
    constexpr auto source = CtString(R"(
        while(true) {
            return 5;
        }
    )");
    constexpr auto ast = parse_tokens<Lexer<source>>();

    static_assert(
        std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                BlockStmt<
                    WhileStmt<
                        BoolLiteralExpr<true>,
                        BlockStmt<
                            ReturnStmt<IntegerLiteralExpr<5>>
                        >
                    >
                >
            >
        >
    );
}

TEST_CASE("parse function", "[constexpr][parser][function]") {
    SECTION("Anon function") {
        constexpr auto source = CtString(R"(
            fn(a: int, b: string, c: bool) -> int {
                return 5;
            }
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        AnonFunctionDecl<
                            Type<TypeKind::int_>,
                            std::tuple<
                                ArgType<Type<TypeKind::int_>, IdentifierExpr<"a">>,
                                ArgType<Type<TypeKind::string>, IdentifierExpr<"b">>,
                                ArgType<Type<TypeKind::bool_>, IdentifierExpr<"c">>
                            >,
                            BlockStmt<
                                ReturnStmt<IntegerLiteralExpr<5>>
                            >
                        >
                    >
                >
            >
        );
    }

    SECTION("function declaration") {
        constexpr auto source = CtString(R"(
            fn add(a: int, b: int) -> int {
                return a;
            }
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        FunctionDecl<
                            IdentifierExpr<"add">,
                            Type<TypeKind::int_>,
                            std::tuple<
                                ArgType<Type<TypeKind::int_>, IdentifierExpr<"a">>,
                                ArgType<Type<TypeKind::int_>, IdentifierExpr<"b">>
                            >,
                            BlockStmt<
                                ReturnStmt<IdentifierExpr<"a">>
                            >
                        >
                    >
                >
            >
        );
    }

    SECTION("function declaration with function type in it") {
        constexpr auto source = CtString(R"(
            fn add(a: int, b: int, c: fn(int, f: int) -> int) -> int {
                return a;
            }
        )");

        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        FunctionDecl<
                            IdentifierExpr<"add">,
                            Type<TypeKind::int_>,
                            std::tuple<
                                ArgType<Type<TypeKind::int_>, IdentifierExpr<"a">>,
                                ArgType<Type<TypeKind::int_>, IdentifierExpr<"b">>,
                                ArgType<
                                    Type<
                                        TypeKind::fn,
                                        Type<TypeKind::int_>,
                                        std::tuple<
                                            ArgType<Type<TypeKind::int_>, void>,
                                            ArgType<Type<TypeKind::int_>, IdentifierExpr<"f">>
                                        >
                                    >,
                                    IdentifierExpr<"c">
                                >
                            >,
                            BlockStmt<
                                ReturnStmt<IdentifierExpr<"a">>
                            >
                        >
                    >
                >
            >
        );
    }
}

