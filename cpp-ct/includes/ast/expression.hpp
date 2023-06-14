#if !defined(MK_EXPRESSION_HPP)
#define MK_EXPRESSION_HPP

#include <utils/ct_string.hpp>
#include <ast/types.hpp>

namespace mk {
    struct ASTNode;

    // enum class ExpressionKind {
    //     unknown,
    //     binary,
    //     unary,
    //     call,
    //     identifier,
    //     literal,
    // };

    template<typename T>
    struct Expr {
        using type = T;
    };

    template<typename L, typename R, typename Op>
    struct BinaryExpr {
        using left = L;
        using right = R;
        using op = Op;
    };

    template<typename T, typename Op>
    struct UnaryExpr {
        using operand = T;
        using op = Op;
    };

    template<typename T, typename Node>
    struct ArgExpr {
        using type = T;
        using node = Node;
    };
    
    template<typename Node, typename... args>
    struct CallExpr {
        using callee = Node;
        using arguments = std::tuple<args...>;
    };

    template<CtString VarName>
    struct IdentifierExpr {
        static constexpr auto var_name = VarName;
    };

    template<typename Type, auto&& val>
    struct LiteralExpr {
        using type = Type;
        static constexpr auto value = val;
    };

} // namespace mk


#endif // MK_EXPRESSION_HPP
