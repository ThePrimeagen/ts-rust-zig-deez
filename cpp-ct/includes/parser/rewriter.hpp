#if !defined(MK_PARSER_REWRITER_HPP)
#define MK_PARSER_REWRITER_HPP

#include <ast/ast_node.hpp>
#include <ast/expression.hpp>
#include <ast/decl.hpp>
#include <ast/statement.hpp>
#include <ast/types.hpp>
#include <ostream>
#include <iomanip>

namespace mk {

    static constexpr int indent_width_v = 4;

    namespace detail {

        template<typename T>
        constexpr auto initialize_value() noexcept {
            if constexpr(std::is_void_v<T>) return BlockStmt<>{};
            else return T{};
        }

        template<CtString V>
        std::ostream& rewriter_identifier_helper(std::ostream& os, IdentifierExpr<V>) {
            os << V;
            return os;
        }
        
        template<std::int64_t V>
        std::ostream& rewriter_integer_literal_helper(std::ostream& os, IntegerLiteralExpr<V>) {
            os << to_string(TypeKind::int_) << '(' << V << ')';
            return os;
        }
        
        template<bool V>
        std::ostream& rewriter_bool_literal_helper(std::ostream& os, BoolLiteralExpr<V>) {
            os << to_string(TypeKind::bool_) << '(' << (V ? "true" : "false") << ')';
            return os;
        }
        
        template<CtString V>
        std::ostream& rewriter_string_literal_helper(std::ostream& os, StringLiteralExpr<V>) {
            os << to_string(TypeKind::string) << "(\"" << V << "\")";
            return os;
        }

        template<typename T>
        std::ostream& rewriter_helper(std::ostream& os, T block, int depth, bool use_indentation) {
            if constexpr (is_block_stmt<T>::value) {
                if constexpr(T::size != 0) {
                    auto helper = []<typename U, typename...Us>(std::ostream& os, BlockStmt<U, Us...>, int depth, bool use_indentation) {
                        if constexpr (!std::is_void_v<U>) {
                            const auto indent = depth * indent_width_v * static_cast<int>(use_indentation);
                            os << std::setw(indent) << '{' << '\n';
                            rewriter_helper(os, detail::initialize_value<U>(), depth + 1, use_indentation) << '\n';
                            os << std::setw(indent) << '}';
                        }
                        if constexpr (sizeof... (Us) > 0) {
                            rewriter_helper(os, detail::initialize_value<BlockStmt<Us...>>(), depth + 1, use_indentation);
                        }
                    };
                    helper(os, block, depth, use_indentation);
                }
            } else if constexpr (is_if_stmt<T>::value) {
                auto const indent = (depth + 1) * indent_width_v * static_cast<int>(use_indentation);
                os << std::setw(indent) << "if (";
                rewriter_helper(os, initialize_value<typename T::condition>(), depth, false) << ')' << '\n';
                rewriter_helper(os, initialize_value<typename T::then>(), depth, use_indentation) << '\n';
                if constexpr (!std::is_void_v<typename T::else_>) {
                    os << std::setw(indent) << "else";
                    rewriter_helper(os, initialize_value<typename T::else_>(), depth, use_indentation);
                }
            } else if constexpr (is_declaration_stmt<T>::value) {
                auto const indent = (depth + 1) * indent_width_v * static_cast<int>(use_indentation);
                os << std::setw(indent) << "let "; 
                rewriter_helper(os, initialize_value<typename T::identifier>(), depth, use_indentation) << " = ";
                rewriter_helper(os, initialize_value<typename T::expression>(), depth, use_indentation);
                os << ';';
            } else if constexpr (is_identifier_expr<T>::value) {
                rewriter_identifier_helper(os, block);
            } else if constexpr (is_string_literal_expr<T>::value || is_bool_literal_expr<T>::value || is_integer_literal_expr<T>::value) {
                if constexpr(is_string_literal_expr<T>::value) {
                    rewriter_string_literal_helper(os, block);
                } else if constexpr(is_bool_literal_expr<T>::value) {
                    rewriter_bool_literal_helper(os, block);
                } else if constexpr(is_integer_literal_expr<T>::value) {
                    rewriter_integer_literal_helper(os, block);
                }
            } else if constexpr(is_array_literal_expr<T>::value) {
                auto helper = []<typename E, typename... Es>(std::ostream& os, ArrayLiteralExpr<E, Es...>, int depth, bool use_indentation) {
                    os << '[';
                    rewriter_helper(os, initialize_value<E>(), depth, use_indentation);
                    if constexpr (sizeof...(Es) > 0) {
                        ((os << ", ", rewriter_helper(os, initialize_value<Es>(), depth, use_indentation)), ...);
                    }
                    os << ']';
                };
                helper(os, block, depth, use_indentation);
            } else if constexpr (is_expr_v<T>) {
                rewriter_helper(os, initialize_value<typename T::type>(), depth, use_indentation);
            }
            return os;
        }

    } // namespace detail
    
    
    template<typename T>
    std::ostream& rewriter(std::ostream& os, ProgramNode<T>, bool use_indentation = false) {
        detail::rewriter_helper(os, detail::initialize_value<T>(), 0, use_indentation) << '\n';
        return os;
    }

} // namespace mk


#endif // MK_PARSER_REWRITER_HPP
