#ifndef MK_SYNTAX_TOKENS_HPP
#define MK_SYNTAX_TOKENS_HPP

#include <cstdint>
#include <string_view>
#include <ostream>
#include <algorithm>
#include <functional>
#include <numeric>
#include <utils/ct_string.hpp>

namespace mk {
    
    enum class TokenKind: std::uint8_t {
        #define MK_TOKEN(name) name,
        #include "syntax/tokens.def"
        
        num_tokens
    };

    constexpr std::string_view to_string(TokenKind kind) noexcept {
        switch (kind) {
            #define MK_TOKEN(name) case TokenKind::name: return #name;
            #include "syntax/tokens.def"
            default: return "unknown";
        }
    }

    struct Token {
        using position_type = unsigned;

        constexpr Token() noexcept = default;
        constexpr Token(Token const&) noexcept = default;
        constexpr Token(Token&&) noexcept = default;
        constexpr Token& operator=(Token const&) noexcept = default;
        constexpr Token& operator=(Token&&) noexcept = default;
        constexpr ~Token() noexcept = default;

        constexpr Token(TokenKind kind, std::string_view lexeme, position_type line = 0, position_type column = 0) noexcept
            : m_kind(kind)
            , m_lexeme(lexeme)
            , m_line(line)
            , m_column(column)
        {}

        [[nodiscard]] constexpr TokenKind kind() const noexcept {
            return m_kind;
        }

        [[nodiscard]] constexpr std::string_view lexeme() const noexcept {
            return m_lexeme;
        }

        [[nodiscard]] constexpr bool is(TokenKind kind) const noexcept {
            return m_kind == kind;
        }

        [[nodiscard]] constexpr bool is_not(TokenKind kind) const noexcept {
            return !is(kind);
        }

        template<typename... Args>
        [[nodiscard]] constexpr bool is_one_of(Args&&... args) const noexcept {
            return (... || is(std::forward<Args>(args)));
        }

        #define MK_TOKEN(name) \
            [[nodiscard]] constexpr bool is_##name() const noexcept { \
                return is(TokenKind::name); \
            }
        #include "syntax/tokens.def"

        constexpr bool operator==(Token const& other) const noexcept {
            return m_kind == other.m_kind && m_lexeme == other.m_lexeme;
        }

        constexpr bool operator!=(Token const& other) const noexcept {
            return !(*this == other);
        }

        [[nodiscard]] constexpr position_type line() const noexcept {
            return m_line;
        }
        
        [[nodiscard]] constexpr position_type column() const noexcept {
            return m_column;
        }

    private:
        TokenKind m_kind{TokenKind::illegal};
        std::string_view m_lexeme;
        position_type m_line{};
        position_type m_column{};
    };


    constexpr bool is_unary_token(TokenKind tk) noexcept {
        switch (tk) {
            case TokenKind::exclamation_mark:
            case TokenKind::minus:
            case TokenKind::plus:
            case TokenKind::tilde: return true;
            default: return false;
        }
    }

    constexpr bool is_binary_token(TokenKind tk) noexcept {
        switch (tk) {
            case TokenKind::ampersand:
            case TokenKind::ampersand_ampersand:
            case TokenKind::caret:
            case TokenKind::equal_equal:
            case TokenKind::greater_than:
            case TokenKind::less_than:
            case TokenKind::minus:
            case TokenKind::not_equal:
            case TokenKind::percent:
            case TokenKind::pipe:
            case TokenKind::pipe_pipe:
            case TokenKind::plus:
            case TokenKind::slash:
            case TokenKind::star: return true;
            default: return false;
        }
    }

} // namespace mk

std::ostream& operator<<(std::ostream& os, mk::TokenKind kind) {
    return os << mk::to_string(kind);
}

std::ostream& operator<<(std::ostream& os, mk::Token const& token) {
    return os << "Token(" << mk::to_string(token.kind()) << ", '" << token.lexeme() << "', <" << token.line() + 1<< ", " << token.column() + 1 <<">" ")";
}

#endif // MK_SYNTAX_TOKENS_HPP
