#if !defined(MK_TYPE_TRAITS_HPP)
#define MK_TYPE_TRAITS_HPP

#include <ast/statement.hpp>
#include <parser/parser_token.hpp>

namespace mk {

    template<detail::ParserToken... T>
    struct TokenList {
        static constexpr auto size = sizeof...(T);
    };

    template<typename ProgramList, typename TokenList>
    struct ParserResult {
        using program_list = ProgramList;
        using token_list = TokenList;
    };

    namespace detail {

        template<typename T>
        struct is_parser_result : std::false_type {};

        template<typename ProgramList, typename TokenList>
        struct is_parser_result<ParserResult<ProgramList, TokenList>> : std::true_type {};


    } // namespace detail

    template<typename T>
    concept ParserResultType = detail::is_parser_result<T>::value;

    template<ParserResultType P>
    using first_parser_result_t = typename P::program_list;

    template<ParserResultType P>
    using second_parser_result_t = typename P::token_list;
    
    template<typename T, typename...Ts>
    constexpr auto push_to_block_list(BlockStmt<Ts...>, T) noexcept {
        return BlockStmt<Ts..., T>{};
    };

    template<detail::ParserToken T, detail::ParserToken... Ts>
    constexpr auto dequeue_token_list(TokenList<T, Ts...>) noexcept {
        return TokenList<Ts...>{};
    };

    template<std::size_t I, detail::ParserToken T, detail::ParserToken... Ts>
    constexpr auto get_element_at_from_token_list(TokenList<T, Ts...>) noexcept {
        if constexpr(I == 0) {
            return T;
        } else {
            return get_element_at_from_token_list<I - 1>(TokenList<Ts...>{});
        }
    }

    template<std::size_t I, detail::ParserToken... Ts>
    constexpr decltype(auto) slice_token_list(TokenList<Ts...> ts) noexcept {
        if constexpr (sizeof...(Ts) <= I || I == 0) {
            return ts;
        } else {
            return slice_token_list<I - 1>(decltype(dequeue_token_list(ts)){});
        }
    };

} // namespace mk


#endif // MK_TYPE_TRAITS_HPP
