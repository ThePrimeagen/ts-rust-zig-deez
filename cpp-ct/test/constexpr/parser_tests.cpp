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

TEST_CASE("parse unary operation", "[constexpr][parser][unary]") {
    constexpr auto source = CtString(R"(
        -5;
        ~4;
        !true;
        +3;
        ----5;
    )");
    constexpr auto ast = parse_tokens<Lexer<source>>();

    static_assert(
        std::is_same_v<
            std::decay_t<decltype(ast)>,
            ProgramNode<
                BlockStmt<
                    UnaryExpr<
                        TokenKind::minus,
                        IntegerLiteralExpr<5>
                    >,
                    UnaryExpr<
                        TokenKind::tilde,
                        IntegerLiteralExpr<4>
                    >,
                    UnaryExpr<
                        TokenKind::exclamation_mark,
                        BoolLiteralExpr<true>
                    >,
                    UnaryExpr<
                        TokenKind::plus,
                        IntegerLiteralExpr<3>
                    >,
                    UnaryExpr<
                        TokenKind::minus,
                        UnaryExpr<
                            TokenKind::minus,
                            UnaryExpr<
                                TokenKind::minus,
                                UnaryExpr<
                                    TokenKind::minus,
                                    IntegerLiteralExpr<5>
                                >
                            >
                        >
                    >
                >
            >
        >
    );
}


TEST_CASE("parse binary operation", "[constexpr][parser][binary]") {
    SECTION("simple operations") {

        constexpr auto source = CtString(R"(
            5 + 5;
            4 - 4;
            3 * 3;
            2 / 2;
            1 % 1;
            0 == 0;
            1 != 1;
            2 < 2;
            3 > 3;
            4 <= 4;
            5 >= 5;
            true && true;
            false || false;
        )");
        constexpr auto ast = parse_tokens<Lexer<source>>();

        static_assert(
            std::is_same_v<
                std::decay_t<decltype(ast)>,
                ProgramNode<
                    BlockStmt<
                        BinaryExpr<
                            TokenKind::plus,
                            IntegerLiteralExpr<5>,
                            IntegerLiteralExpr<5>
                        >,
                        BinaryExpr<
                            TokenKind::minus,
                            IntegerLiteralExpr<4>,
                            IntegerLiteralExpr<4>
                        >,
                        BinaryExpr<
                            TokenKind::star,
                            IntegerLiteralExpr<3>,
                            IntegerLiteralExpr<3>
                        >,
                        BinaryExpr<
                            TokenKind::slash,
                            IntegerLiteralExpr<2>,
                            IntegerLiteralExpr<2>
                        >,
                        BinaryExpr<
                            TokenKind::percent,
                            IntegerLiteralExpr<1>,
                            IntegerLiteralExpr<1>
                        >,
                        BinaryExpr<
                            TokenKind::equal_equal,
                            IntegerLiteralExpr<0>,
                            IntegerLiteralExpr<0>
                        >,
                        BinaryExpr<
                            TokenKind::not_equal,
                            IntegerLiteralExpr<1>,
                            IntegerLiteralExpr<1>
                        >,
                        BinaryExpr<
                            TokenKind::less_than,
                            IntegerLiteralExpr<2>,
                            IntegerLiteralExpr<2>
                        >,
                        BinaryExpr<
                            TokenKind::greater_than,
                            IntegerLiteralExpr<3>,
                            IntegerLiteralExpr<3>
                        >,
                        BinaryExpr<
                            TokenKind::less_than_equal,
                            IntegerLiteralExpr<4>,
                            IntegerLiteralExpr<4>
                        >,
                        BinaryExpr<
                            TokenKind::greater_than_equal,
                            IntegerLiteralExpr<5>,
                            IntegerLiteralExpr<5>
                        >,
                        BinaryExpr<
                            TokenKind::ampersand_ampersand,
                            BoolLiteralExpr<true>,
                            BoolLiteralExpr<true>
                        >,
                        BinaryExpr<
                            TokenKind::pipe_pipe,
                            BoolLiteralExpr<false>,
                            BoolLiteralExpr<false>
                        >
                    >
                >
            >
        );
    }
}

