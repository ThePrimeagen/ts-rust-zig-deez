#if !defined(MK_LEXER_LEXER_HPP)
#define MK_LEXER_LEXER_HPP

#include <syntax/tokens.hpp>
#include <iostream>

namespace mk {

    namespace detail {

        [[nodiscard]] constexpr bool is_alpha(char c) noexcept {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
        }
        
        [[nodiscard]] constexpr bool is_num(char c) noexcept {
            return c >= '0' && c <= '9';
        }
        
        [[nodiscard]] constexpr bool is_newline(char c) noexcept {
            return c == '\n' || c == '\r';
        }
        
        [[nodiscard]] constexpr bool is_space(char c) noexcept {
            return c == ' ' || c == '\t' || c == '\v' || c == '\f';
        }
        
        [[nodiscard]] constexpr bool is_whitespace(char c) noexcept {
            return is_space(c) || is_newline(c);
        }

    } // namespace detail

    template<std::size_t N>
    struct Lexer {
        using source_type       = CtString<N>;
        using value_type        = typename source_type::value_type;
        using size_type         = std::size_t;
        using difference_type   = std::ptrdiff_t;
        using position_type     = typename Token::position_type;

        constexpr Lexer(source_type source) noexcept
            : m_source(source)
        {
            cache_newlines_for_lookup();
        }

        constexpr Lexer(Lexer const&) noexcept = default;
        constexpr Lexer(Lexer&&) noexcept = default;
        constexpr Lexer& operator=(Lexer const&) noexcept = default;
        constexpr Lexer& operator=(Lexer&&) noexcept = default;
        constexpr ~Lexer() noexcept = default;

        [[nodiscard]] constexpr bool eof() const noexcept {
            return m_current_cursor >= m_source.size();
        }

        [[nodiscard]] constexpr value_type current() const noexcept {
            return m_source[m_current_cursor];
        }

        [[nodiscard]] constexpr value_type peek(size_type k = 1u) const noexcept {
            auto const index = std::min(cursor_position() + k, m_source.size());
            if (index == m_source.size()) return static_cast<value_type>(0);
            return m_source[index];
        }

        constexpr void skip(size_type n = 1u) noexcept {
            set_cursor(std::min(cursor_position() + n, m_source.size()));
        }

        constexpr void skip_while(auto&& predicate) noexcept {
            while (!eof() && predicate(current())) {
                skip();
            }
        }

        constexpr void skip_whitespace() noexcept {
            skip_while([](auto c) { return detail::is_whitespace(c); });
        }

        [[nodiscard]] constexpr Token match_punctuation() noexcept {
            skip_whitespace();
            auto const token = match_punctuation_helper();
            if (!token.is_unknown()) skip(token.lexeme().size());
            return token;
        }

        [[nodiscard]] constexpr Token next() noexcept {
            {
                // Keep the whitespace so that we can recreate the source code
                auto token = try_lex_whitespace();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = try_lex_comment();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = try_lex_string_literal();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = match_punctuation();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = try_lex_keyword();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = try_lex_number();
                if (!token.is_unknown()) return token;
            }
            {
                auto token = try_lex_identifier();
                if (!token.is_unknown()) return token;
            }
            return Token{ TokenKind::illegal, "illegal" };
        }

        [[nodiscard]] constexpr size_type cursor_position() const noexcept {
            return m_current_cursor;
        }
        
        template<std::size_t M>
        [[nodiscard]] constexpr auto lex(std::array<Token, M>& tokens, bool skip_whitespace = true) noexcept {
            size_type index{};

            while(index < tokens.size()) {
                auto token = next();
                if (skip_whitespace && (token.is_one_of(TokenKind::whitespace, TokenKind::newline))) continue;
                tokens[index++] = token;
            }
        }
        
        [[nodiscard]] constexpr auto lex(bool skip_whitespace = true) noexcept {
            std::array<Token, N> tokens{};
            lex(tokens, skip_whitespace);
            return tokens;
        }


    private:

            constexpr void set_cursor(size_type k) noexcept {
                m_current_cursor = k;
            }

            [[nodiscard]] constexpr Token make_unknown_token() const noexcept {
                return Token{ TokenKind::unknown, "unknown" };
            }

            [[nodiscard]] constexpr Token make_eof_token() const noexcept {
                auto const [line, col] = calculate_line_and_col(cursor_position());
                return Token{ TokenKind::eof, "EOF", line, col };
            }

            [[nodiscard]] constexpr Token try_lex_whitespace() noexcept {
                if (eof()) return make_eof_token();
                
                auto newline = try_lex_newline();
                if (!newline.is_unknown()) return newline;

                return try_lex_space();
            }
            
            [[nodiscard]] constexpr Token try_lex_comment() noexcept {
                if (eof()) return make_eof_token();
                
                auto single_line = try_lex_single_line_comment();
                if (!single_line.is_unknown()) return single_line;

                return try_lex_multi_line_comment();
            }
            
            [[nodiscard]] constexpr Token try_lex_string_literal() noexcept {
                if (eof()) return make_eof_token();
                
                if (current() != '"') return make_unknown_token();
                auto [line, col] = calculate_line_and_col();
                skip();
                auto const start = cursor_position();
                auto end = start;
                while (!eof()) {
                    if (current() == '\\') {
                        skip(2);
                        end += 2;
                        continue;
                    }
                    if (current() == '"') {
                        skip();
                        break;
                    }
                    ++end;
                    skip();
                }
                return Token{ TokenKind::string_literal, m_source.view(start, end - start), line, col };
            }

            [[nodiscard]] constexpr Token try_lex_single_line_comment() noexcept {
                if (current() == '/' && peek() == '/') {
                    auto [line, col] = calculate_line_and_col();
                    skip(2);
                    auto const [start, end] = match_while([](auto c) { return !detail::is_newline(c); });
                    return Token{ TokenKind::comment, m_source.view(start, end - start), line, col };
                }
                return make_unknown_token();
            }
            
            [[nodiscard]] constexpr Token try_lex_multi_line_comment() noexcept {
                if (current() == '/' && peek() == '*') {
                    auto [line, col] = calculate_line_and_col();
                    skip(2);
                    auto const start = cursor_position();
                    auto end = start;
                    while (!eof()) {
                        if (current() == '*' && peek() == '/') {
                            skip(2);
                            break;
                        }
                        ++end;
                        skip();
                    }
                    return Token{ TokenKind::comment, m_source.view(start, end - start), line, col };
                }
                return make_unknown_token();
            }
            
            [[nodiscard]] constexpr Token try_lex_newline() noexcept {
                auto [line, col] = calculate_line_and_col();
                auto const [start, end] = match_while([](auto c) { return detail::is_newline(c); });
                if (start == end) return make_unknown_token();
                return Token{ TokenKind::newline, m_source.view(start, end - start), line, col };
            }
            
            [[nodiscard]] constexpr Token try_lex_space() noexcept {
                auto [line, col] = calculate_line_and_col();
                auto const [start, end] = match_while([](auto c) { return detail::is_space(c); });
                if (start == end) return make_unknown_token();
                return Token{ TokenKind::whitespace, m_source.view(start, end - start), line, col };
            }
            
            template<typename Fn>
            [[nodiscard]] constexpr std::pair<size_type, size_type> match_while(Fn&& fn) noexcept {
                auto const start = cursor_position();
                skip_while(std::forward<Fn>(fn));
                auto const end = cursor_position();
                return { start, end };
            }

            [[nodiscard]] constexpr Token match_punctuation_helper() const noexcept {
                if (eof()) return make_eof_token();
                auto [line, col] = calculate_line_and_col();
                {
                    #define MK_PUNCTUATOR_MULTI_CHAR(name, str, lookahead) \
                        if (m_source.view(cursor_position(), lookahead + 1) == str) { \
                            return Token{ TokenKind::name, str, line, col }; \
                        }
                        
                    #include <syntax/tokens.def>
                }
                
                {
                    switch(current()) {
                        #define MK_PUNCTUATOR_SINGLE_CHAR(name, c) \
                            case c: return Token{ TokenKind::name, m_source.view(cursor_position(), 1), line, col };
                        #include <syntax/tokens.def>
                    }

                }
                return make_unknown_token();
            }

        [[nodiscard]] constexpr Token try_lex_identifier() noexcept {
            if (eof()) return make_eof_token();
            if (!(detail::is_alpha(current()) || current() == '_')) return make_unknown_token();

            auto [line, col] = calculate_line_and_col();

            auto const [start, end] = match_while([](auto c) { return detail::is_alpha(c) || detail::is_num(c) || c == '_'; });
            
            if (start == end) return make_unknown_token();

            return Token{ TokenKind::identifier, m_source.view(start, end - start), line, col };
        }

        [[nodiscard]] constexpr Token try_lex_number() noexcept {
            if (eof()) return make_eof_token();
            
            auto [line, col] = calculate_line_and_col();

            auto const [start, end] = match_while([](auto c) { return detail::is_num(c); });

            if (start == end) return make_unknown_token();

            return Token{ TokenKind::int_literal, m_source.view(start, end - start), line, col };
        }

        [[nodiscard]] constexpr Token try_lex_keyword() noexcept {
            if (eof()) return make_eof_token();

            auto [line, col] = calculate_line_and_col();

            auto const [start, end] = match_while([](auto c) { return detail::is_alpha(c); });

            if (start == end) return make_unknown_token();

            auto const lexeme = m_source.view(start, end - start);

            if (lexeme == "true") return Token{ TokenKind::bool_literal, lexeme, line, col };
            else if (lexeme == "false") return Token{ TokenKind::bool_literal, lexeme, line, col };

            #define MK_KEYWORD(name) \
                if (lexeme == #name) return Token{ TokenKind::kw_## name, #name, line, col };
            
            #include <syntax/tokens.def>

            set_cursor(start);

            return make_unknown_token();
        }

        constexpr void cache_newlines_for_lookup() noexcept {
            m_cached_newlines_size = 1;
            m_cached_newlines[0] = 0;
            for (size_type i{1}; i < m_source.size(); ++i) {
                if (m_source[i] == '\n') {
                    m_cached_newlines[m_cached_newlines_size++] = static_cast<position_type>(i);
                }
            }
        }
        
        // NOTE: We should use linear search since the cache is small and binary search for large caches
        [[nodiscard]] constexpr size_type get_newline_position(size_type char_pos) const noexcept {
            for (size_type i{1}; i < m_cached_newlines_size; ++i) {
                if (m_cached_newlines[i] > char_pos) {
                    return i - 1u;
                }
            }
            return std::max(static_cast<unsigned>(m_cached_newlines_size), 1u) - 1u;
        }

        [[nodiscard]] constexpr std::pair<position_type, position_type> calculate_line_and_col() const noexcept {
            auto const char_pos = cursor_position();
            return calculate_line_and_col(char_pos);
        }
        
        [[nodiscard]] constexpr std::pair<position_type, position_type> calculate_line_and_col(size_type char_pos) const noexcept {
            auto const line = get_newline_position(char_pos);
            auto const col = char_pos - m_cached_newlines[line] - 1;
            return { static_cast<position_type>(line), col };
        }

    private:
        source_type m_source;
        size_type m_current_cursor{};
        std::array<position_type, N + 1> m_cached_newlines{};
        size_type m_cached_newlines_size{};
    };

} // namespace mk

#endif // MK_LEXER_LEXER_HPP
