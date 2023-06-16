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

    template<std::int64_t val>
    struct IntegerLiteralExpr {
        static constexpr auto value = val;
    };
    
    template<bool val>
    struct BoolLiteralExpr {
        static constexpr auto value = val;
    };
    
    template<CtString val>
    struct StringLiteralExpr {
        static constexpr auto value = val;
    };
    
    template<typename... Es>
    struct ArrayLiteralExpr {
        using elements = std::tuple<Es...>;
    };

    namespace detail {
        
        template<typename T>
        struct is_expr : std::false_type {};

        template<typename T>
        struct is_expr<Expr<T>> : std::true_type {};

        template<typename T>
        constexpr bool is_expr_v = is_expr<T>::value;

        template<typename T>
        struct is_identifier_expr : std::false_type {};

        template<CtString V>
        struct is_identifier_expr<IdentifierExpr<V>> : std::true_type {};

        template<typename T>
        constexpr bool is_identifier_expr_v = is_identifier_expr<T>::value;

        template<typename T>
        struct is_integer_literal_expr : std::false_type {};

        template<std::int64_t V>
        struct is_integer_literal_expr<IntegerLiteralExpr<V>> : std::true_type {};

        template<typename T>
        constexpr bool is_integer_literal_expr_v = is_integer_literal_expr<T>::value;

        template<typename T>
        struct is_bool_literal_expr : std::false_type {};

        template<bool V>
        struct is_bool_literal_expr<BoolLiteralExpr<V>> : std::true_type {};

        template<typename T>
        constexpr bool is_bool_literal_expr_v = is_bool_literal_expr<T>::value;

        template<typename T>
        struct is_string_literal_expr : std::false_type {};

        template<CtString V>
        struct is_string_literal_expr<StringLiteralExpr<V>> : std::true_type {};

        template<typename T>
        constexpr bool is_string_literal_expr_v = is_string_literal_expr<T>::value;

        template<typename T>
        struct is_array_literal_expr : std::false_type {};

        template<typename... Es>
        struct is_array_literal_expr<ArrayLiteralExpr<Es...>> : std::true_type {};

        template<typename T>
        constexpr bool is_array_literal_expr_v = is_array_literal_expr<T>::value;

        template<typename T>
        struct is_binary_expr : std::false_type {};

        template<typename L, typename R, typename Op>
        struct is_binary_expr<BinaryExpr<L, R, Op>> : std::true_type {};

        template<typename T>
        constexpr bool is_binary_expr_v = is_binary_expr<T>::value;

        template<typename T>
        struct is_unary_expr : std::false_type {};

        template<typename T, typename Op>
        struct is_unary_expr<UnaryExpr<T, Op>> : std::true_type {};

        template<typename T>
        constexpr bool is_unary_expr_v = is_unary_expr<T>::value;

        template<typename T>
        struct is_call_expr : std::false_type {};

        template<typename Node, typename... args>
        struct is_call_expr<CallExpr<Node, args...>> : std::true_type {};

        template<typename T>
        constexpr bool is_call_expr_v = is_call_expr<T>::value;

        template<typename T>
        struct is_arg_expr : std::false_type {};

        template<typename T, typename Node>
        struct is_arg_expr<ArgExpr<T, Node>> : std::true_type {};

        template<typename T>
        constexpr bool is_arg_expr_v = is_arg_expr<T>::value;

    } // namespace detail

} // namespace mk


#endif // MK_EXPRESSION_HPP
