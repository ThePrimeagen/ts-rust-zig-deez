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


    template<typename T>
    struct BlockStmt {
        using type = T;
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

    template<typename Type, typename Expr>
    struct DeclarationStmt {
        using type = Type;
        using expression = Expr;
    };

} // namespace mk


#endif // MK_AST_STATEMENT_HPP
