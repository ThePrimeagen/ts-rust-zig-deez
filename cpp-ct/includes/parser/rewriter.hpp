#if !defined(MK_PARSER_REWRITER_HPP)
#define MK_PARSER_REWRITER_HPP

#include <ast/ast_node.hpp>
#include <ast/expression.hpp>
#include <ast/decl.hpp>
#include <ast/statement.hpp>
#include <ast/types.hpp>
#include <ostream>

namespace mk {

    namespace detail {
        template<typename T>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, Expr<T>);

        template<CtString V>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, IdentifierExpr<V>) {
            os << V;
            return os;
        }
        
        template<auto V>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, IntegerLiteralExpr<V>) {
            os << to_string(TypeKind::int_) << '(' << V << ')';
            return os;
        }
        
        template<CtString V>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, StringLiteralExpr<V>) {
            os << to_string(TypeKind::string) << "(\"" << V << "\")";
            return os;
        }
        
        template<typename E, typename... Es>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, ArrayLiteralExpr<E, Es...>) {
            os << '[';
            rewriter_helper(os, E{});
            if constexpr (sizeof...(Es) > 0) {
                ((os << ", ", rewriter_helper(os, Es{})), ...);
            }
            os << ']';
            return os;
        }

        template<typename T>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, Expr<T>) {
            rewriter_helper(os, T{});
            return os;
        }

        template<typename T, typename I, typename E>
        constexpr decltype(auto) rewriter_helper(std::ostream& os, DeclarationStmt<T, I, E>) {
            os << "let "; 
            rewriter_helper(os, I{}) << " = ";
            rewriter_helper(os, E{});
            os << ';' << '\n';
            return os;
        }

    } // namespace detail
    
    
    template<typename... Ts>
    constexpr decltype(auto) rewriter(std::ostream& os, ProgramNode<Ts...>) {
        (detail::rewriter_helper(os, Ts{}), ...);
        return os;
    }

} // namespace mk


#endif // MK_PARSER_REWRITER_HPP
