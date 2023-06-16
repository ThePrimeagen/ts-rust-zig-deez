#if !defined(MK_AST_STATEMENT_HPP)
#define MK_AST_STATEMENT_HPP

namespace mk {
    
    // enum class StatementKind {
    //     unknown,
    //     block,
    //     if_,
    //     while_,
    //     return_,
    //     expression,
    //     declaration,
    // };


    template<typename... Ts>
    struct BlockStmt {
        using type = std::tuple<Ts...>;
        static constexpr auto size = sizeof...(Ts);
    };
    

    template<typename Cond, typename Then, typename Else>
    struct IfStmt {
        using condition = Cond;
        using then = Then;
        using else_ = Else;
    };

    template<typename Cond, typename Body>
    struct WhileStmt {
        using condition = Cond;
        using body = Body;
    };

    template<typename Expr>
    struct ReturnStmt {
        using expression = Expr;
    };

    template<typename Expr>
    struct ExpressionStmt {
        using expression = Expr;
    };

    template<typename Type, typename Identifier, typename Expr>
    struct DeclarationStmt {
        using type = Type;
        using identifier = Identifier;
        using expression = Expr;
    };

    namespace detail {
        
        template<typename T>
        struct is_block_stmt : std::false_type {};

        template<typename... Ts>
        struct is_block_stmt<BlockStmt<Ts...>> : std::true_type {};

        template<typename T>
        static constexpr auto is_block_stmt_v = is_block_stmt<T>::value;

        template<typename T>
        struct is_if_stmt : std::false_type {};

        template<typename Cond, typename Then, typename Else>
        struct is_if_stmt<IfStmt<Cond, Then, Else>> : std::true_type {};

        template<typename T>
        static constexpr auto is_if_stmt_v = is_if_stmt<T>::value;

        template<typename T>
        struct is_while_stmt : std::false_type {};

        template<typename Cond, typename Body>
        struct is_while_stmt<WhileStmt<Cond, Body>> : std::true_type {};

        template<typename T>
        static constexpr auto is_while_stmt_v = is_while_stmt<T>::value;

        template<typename T>
        struct is_return_stmt : std::false_type {};

        template<typename Expr>
        struct is_return_stmt<ReturnStmt<Expr>> : std::true_type {};

        template<typename T>
        static constexpr auto is_return_stmt_v = is_return_stmt<T>::value;

        template<typename T>
        struct is_expression_stmt : std::false_type {};

        template<typename Expr>
        struct is_expression_stmt<ExpressionStmt<Expr>> : std::true_type {};

        template<typename T>
        static constexpr auto is_expression_stmt_v = is_expression_stmt<T>::value;

        template<typename T>
        struct is_declaration_stmt : std::false_type {};

        template<typename Type, typename Identifier, typename Expr>
        struct is_declaration_stmt<DeclarationStmt<Type, Identifier, Expr>> : std::true_type {};

        template<typename T>
        static constexpr auto is_declaration_stmt_v = is_declaration_stmt<T>::value;


    } // namespace detail
    

} // namespace mk


#endif // MK_AST_STATEMENT_HPP
