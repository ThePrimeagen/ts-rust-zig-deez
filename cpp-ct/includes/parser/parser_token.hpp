#if !defined(MK_PARSER_PARSER_TOKEN_HPP)
#define MK_PARSER_PARSER_TOKEN_HPP

#include <syntax/tokens.hpp>

namespace mk {
    
    namespace detail {
        
        template<std::size_t N>
        struct ParserToken {
            TokenKind kind;
            char lexeme[N];
            unsigned line;
            unsigned col;

            constexpr operator std::string_view() const noexcept {
                return std::string_view{lexeme, N};
            }
        };

        template<std::size_t N>
        constexpr auto to_parser_token(Token const& token) {
            ParserToken<N> parser_token{};
            parser_token.kind = token.kind();
            parser_token.line = token.line();
            parser_token.col = token.column();
            std::copy(token.lexeme().begin(), token.lexeme().end(), parser_token.lexeme);
            return parser_token;
        }

    } // namespace detail
    

} // namespace mk


#endif // MK_PARSER_PARSER_TOKEN_HPP
