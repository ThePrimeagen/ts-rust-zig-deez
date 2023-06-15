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
