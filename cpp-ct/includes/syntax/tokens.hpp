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
        constexpr Token() noexcept = default;
        constexpr Token(Token const&) noexcept = default;
        constexpr Token(Token&&) noexcept = default;
        constexpr Token& operator=(Token const&) noexcept = default;
        constexpr Token& operator=(Token&&) noexcept = default;
        constexpr ~Token() noexcept = default;

        constexpr Token(TokenKind kind, std::string_view lexeme) noexcept
            : m_kind(kind), m_lexeme(lexeme)
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

    private:
        TokenKind m_kind{TokenKind::illegal};
        std::string_view m_lexeme;
    };

} // namespace mk

std::ostream& operator<<(std::ostream& os, mk::TokenKind kind) {
    return os << mk::to_string(kind);
}

std::ostream& operator<<(std::ostream& os, mk::Token const& token) {
    return os << "Token(" << mk::to_string(token.kind()) << ", '" << token.lexeme() << "')";
}

#endif // MK_SYNTAX_TOKENS_HPP
