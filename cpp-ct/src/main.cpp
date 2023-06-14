#include <iostream>
#include <array>
#include <lexer/lexer.hpp>

template<std::size_t M, std::size_t N>
constexpr std::array<mk::Token, M> lex_source(mk::CtString<N> source, bool skip_whitespace = true) noexcept {
    auto temp = mk::Lexer(source);
    std::size_t curr_ptr{};
    std::array<mk::Token, M> tokens;

    while (curr_ptr < M) {
        mk::Token token = temp.next();
        if (token.is(mk::TokenKind::unknown)) break;
        if (skip_whitespace && token.is_one_of(mk::TokenKind::whitespace, mk::TokenKind::newline)) continue;
        tokens[curr_ptr++] = token;

        if (token.is_one_of(mk::TokenKind::eof, mk::TokenKind::illegal)) break;
    }
    return tokens;
}

int main() {
    static constexpr auto source = mk::CtString(R"(
        // This is a comment
        /*
            This is a multiline comment
        */
        let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        !-/ *5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        let n = 10;
        n <<= 1;
        let s = "Hello, \"World!\"";
    )");

    constexpr auto tokens = lex_source<1000>(source);
    auto const eof_pos = std::find_if(tokens.begin(), tokens.end(), [](auto const& token) {
        return token.is_eof();
    });

    auto const total_tokens = std::min(static_cast<std::size_t>(std::distance(tokens.begin(), eof_pos)) + 1ul, tokens.size());
    
    std::cout<< "Total tokens: " << total_tokens << '\n';

    for (std::size_t i = 0; i < total_tokens; ++i) {
        std::cout << i << " : " << tokens[i] << '\n';
    }
    return 0;
}
