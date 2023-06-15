#if !defined(MK_PARSER_PARSER_HPP)
#define MK_PARSER_PARSER_HPP

#include <lexer/lexer.hpp>
#include <ast/ast_node.hpp>
#include <ast/expression.hpp>
#include <ast/decl.hpp>
#include <ast/statement.hpp>
#include <ast/types.hpp>
#include <utils/type_traits.hpp>
#include "parser_token.hpp"

namespace mk {

    namespace detail {
        
        [[nodiscard]] constexpr std::int64_t parse_int_literal_value(std::string_view s) noexcept {
            std::int64_t value = 0;
            for (auto c : s) {
                value = value * 10 + (c - '0');
            }
            return value;
        }

        [[nodiscard]] constexpr double parse_floating_point_literal_value(std::string_view s) noexcept {
            double value = 0.0;
            double factor = 0.1;
            for (auto c : s) {
                if (c == '.') {
                    factor = 1.0;
                    continue;
                }
                value += (c - '0') * factor;
                factor /= 10.0;
            }
            return value;
        }

    } // namespace detail
    
    

    namespace detail {

        template<ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_int_literal(TokenList<T, Ts...> ts) {
            static_assert(T.kind == TokenKind::int_literal, "Expected integer literal");
            constexpr auto value = detail::parse_int_literal_value(get_element_at_from_token_list<0>(ts).lexeme);
            return IntegerLiteralExpr<value> {};
        }

        template<ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_literal(TokenList<T, Ts...> ts) {
            if constexpr (T.kind == TokenKind::int_literal) {
                return parse_int_literal(ts);
            } else if constexpr (T.kind == TokenKind::string_literal) {
                return StringLiteralExpr<get_element_at_from_token_list<0>(ts).lexeme> {};
            } else {
                static_assert(T.kind == TokenKind::int_literal, "Expected literal");
            }
        }

        template<ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_binary_expression() {
            
        }

        template<typename E, ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_expression(TokenList<T, Ts...> ts, Expr<E> expr);
        
        template<ParserToken T, ParserToken... Ts, typename... Es>
        [[nodiscard]] constexpr auto parse_array_literal_helper(TokenList<T, Ts...> ts, ArrayLiteralExpr<Es...> arr = ArrayLiteralExpr<>{}) {
            static_assert(T.kind != TokenKind::eof, "Unexpected end of file");

            if constexpr (T.kind == TokenKind::close_bracket) {
                return ParserResult<Expr<ArrayLiteralExpr<Es...>>, TokenList<Ts...>>();
            } else if constexpr (T.kind == TokenKind::comma) {
                return parse_array_literal_helper(dequeue_token_list(ts), arr);
            } else {
                using expr_t = decltype(parse_expression(ts, Expr<void>{}));
                using rest_t = second_parser_result_t<expr_t>;
                using result_t = first_parser_result_t<expr_t>;
                return parse_array_literal_helper(rest_t{}, ArrayLiteralExpr<Es..., result_t>{});
            }
        }
        
        template<ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_array_literal(TokenList<T, Ts...> ts) {
            static_assert(T.kind == TokenKind::open_bracket, "Expected '['");
            return parse_array_literal_helper(dequeue_token_list(ts), ArrayLiteralExpr<>{});
        }

        template<typename E, ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_expression(TokenList<T, Ts...> ts, Expr<E> expr) {
            if constexpr (T.kind == TokenKind::semicolon || T.kind == TokenKind::eof) {
                return ParserResult<decltype(expr), TokenList<Ts...>>();
            } else if constexpr (T.kind == TokenKind::int_literal || T.kind == TokenKind::string_literal) {
                return parse_expression(TokenList<Ts...>{}, Expr<decltype(parse_literal(ts))>{});
            } else if constexpr (T.kind == TokenKind::open_paren ) {
                using inner_expr_t = decltype(parse_expression(dequeue_token_list(ts)));
                using rest_t = second_parser_result_t<inner_expr_t>;
                using result_t = first_parser_result_t<inner_expr_t>;
                constexpr auto closing_pair = get_element_at_from_token_list<0>(rest_t{});
                if constexpr (T.kind == TokenKind::open_paren && closing_pair.kind != TokenKind::close_paren) {
                    // NOTE: We double check because we want lazy evaluation. Otherwise, it'll fail to compile
                    static_assert(T.kind == TokenKind::open_paren, "Expected expression after '('");
                } else {
                    return parse_expression(dequeue_token_list(rest_t{}), Expr<result_t>{});
                }
            } else if constexpr (T.kind == TokenKind::open_bracket) {
                return parse_array_literal(ts);
            } else {
                return ParserResult<decltype(expr), TokenList<T, Ts...>>();
            }
        }

        template<ParserToken T, ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_let_keyword(TokenList<T, Ts...>) {
            // Validate that the first token is a let keyword
            static_assert(T.kind == TokenKind::kw_let, "Expected let keyword");

            constexpr auto ts = TokenList<Ts...>{};
            
            static_assert(sizeof...(Ts) > 0, "Expected identifier after 'let' keyword");
            constexpr auto identifier = get_element_at_from_token_list<0>(ts);
            static_assert(identifier.kind == TokenKind::identifier, "Expected identifier after 'let' keyword");

            static_assert(sizeof...(Ts) > 1, "Expected '=' after identifier");
            constexpr auto assign_op = get_element_at_from_token_list<1>(ts);
            static_assert(assign_op.kind == TokenKind::equal, "Expected equal sign after identifier");

            static_assert(sizeof...(Ts) > 2, "Expected expression after '='");
            using expr_result_t = decltype(parse_expression(slice_token_list<2, Ts...>(ts), Expr<void>{}));

            using stmt_t = DeclarationStmt<Type<TypeKind::int_>, IdentifierExpr<identifier.lexeme>, first_parser_result_t<expr_result_t>>;

            using rest_tokens_t = second_parser_result_t<expr_result_t>;

            return ParserResult<stmt_t, decltype(dequeue_token_list(rest_tokens_t{}))>{};
        }

        template<ParserToken T, ParserToken... Ts, typename... Us>
        [[nodiscard]] constexpr auto parse_stmt(TokenList<T, Ts...> ts, ProgramNode<Us...> list = ProgramNode<>{}) {
            if constexpr (T.kind == TokenKind::kw_let) {
                static_assert(sizeof...(Ts) > 0, "Expected identifier after let keyword");
                using parse_result_t = decltype(parse_let_keyword(ts));

                using result_t = decltype(push_to_program_list(list, first_parser_result_t<parse_result_t>{}));
                
                return ParserResult<result_t, second_parser_result_t<parse_result_t>>{};
            } else {
                return ParserResult<ProgramNode<Us...>, TokenList<T, Ts...>>{};
            }
        }
        
        template<ParserToken T, ParserToken... Ts, typename... Us>
        [[nodiscard]] constexpr auto parse_stmts(TokenList<T, Ts...> ts, ProgramNode<Us...> list = ProgramNode<>{}) {
            using result_t = decltype(parse_stmt(ts, list));

            using list_t = first_parser_result_t<result_t>;
            using remaining_t = second_parser_result_t<result_t>;
            
            if constexpr(remaining_t::size == 0 || T.kind == TokenKind::eof) {
                return list_t{};
            } else {
                return parse_stmts(remaining_t{}, list_t{});
            }
        }

        template<ParserToken... Ts>
        [[nodiscard]] constexpr auto parse_program(TokenList<Ts...> ts) {
            if constexpr (sizeof...(Ts) == 0) {
                return ProgramNode<>{};
            } else {
                using result_t = std::decay_t<decltype(parse_stmts<Ts...>(ts))>;
                return result_t();
            }
        }
    }

    template<LexerType L>
    [[nodiscard]] constexpr auto parse_tokens() {
        constexpr auto max_number_tokens = L{}.lex().size();
        constexpr auto parse_helper = []<std::size_t... I>(std::index_sequence<I...>) {
            constexpr auto tokens = L{}.lex();
            return detail::parse_program(TokenList<detail::to_parser_token<tokens[I].lexeme().size() + 1>(tokens[I])...>{});
        };
        
        return parse_helper(std::make_index_sequence<max_number_tokens>{});
    }

    

} // namespace mk


#endif // MK_PARSER_PARSER_HPP
