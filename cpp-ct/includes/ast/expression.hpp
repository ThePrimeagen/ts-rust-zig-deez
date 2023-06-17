#if !defined(MK_EXPRESSION_HPP)
#define MK_EXPRESSION_HPP

#include <utils/ct_string.hpp>
#include <ast/types.hpp>

namespace mk {

    // enum class ExpressionKind {
    //     unknown,
    //     binary,
    //     unary,
    //     call,
    //     identifier,
    //     literal,
    // };

    struct EmptyExpr {};

    template<TokenKind Op, typename L, typename R>
    struct BinaryExpr {
        using lhs = L;
        using rhs = R;
        static constexpr auto op = Op;
    };

    template<TokenKind Op, typename T>
    struct UnaryExpr {
        using operand = T;
        static constexpr auto op = Op;
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

        template<TokenKind Op, typename L, typename R>
        struct is_binary_expr<BinaryExpr<Op, L, R>> : std::true_type {};

        template<typename T>
        constexpr bool is_binary_expr_v = is_binary_expr<T>::value;

        template<typename T>
        struct is_unary_expr : std::false_type {};

        template<TokenKind Op, typename T>
        struct is_unary_expr<UnaryExpr<Op, T>> : std::true_type {};

        template<typename T>
        constexpr bool is_unary_expr_v = is_unary_expr<T>::value;

        template<typename T>
        struct is_call_expr : std::false_type {};

        template<typename Node, typename... args>
        struct is_call_expr<CallExpr<Node, args...>> : std::true_type {};

        template<typename T>
        constexpr bool is_call_expr_v = is_call_expr<T>::value;

        template<typename T>
        constexpr bool is_expr_v = 
            is_call_expr_v<T> ||
            is_unary_expr_v<T> ||
            is_binary_expr_v<T> || 
            is_identifier_expr_v<T> ||
            is_integer_literal_expr_v<T> ||
            is_bool_literal_expr_v<T> ||
            is_string_literal_expr_v<T> ||
            is_array_literal_expr_v<T> ||
            std::is_same_v<T, EmptyExpr>;

    } // namespace detail

    template<typename T>
    concept Expr = detail::is_expr_v<T>;

} // namespace mk


#endif // MK_EXPRESSION_HPP
