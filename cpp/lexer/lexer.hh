#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <unordered_map>
#include <cstdint>


enum class token_type {
    Identifier,
    Integer,

    Illegal,
    Eof,
    Assign,

    Bang,
    Dash,
    ForwardSlash,
    Asterisk,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,

    Function,
    Let,

    If,
    Else,
    Return,
    True,
    False
};
std::string_view getTokenTypeName(token_type t);


struct token final {
    token_type type;
    std::string_view literal;

    inline token() : type(token_type::Illegal)
    {}
    inline token (const token&) = default;
    inline token(token &&) = default; 
    inline token(token_type t, std::string_view literal_)
        : type(t)
        , literal(literal_)
    {}
    token &operator=(const token &) = default;
    token &operator=(token &&) = default;

    inline void set(token_type t, std::string_view literal_)
    {
        type = t;
        literal = literal_;
    }

    inline void set(token_type t, std::string::iterator iter)
    {
        type = t;
        literal = std::string_view(iter,iter+1);
    }

    std::string to_string();
};


class lexer final {
private:

    // Enable heterogenous lookup in unordered_map, see https://en.cppreference.com/w/cpp/container/unordered_map/find
    struct string_hash
    {
        using hash_type = std::hash<std::string_view>;
        using is_transparent = void;

        std::size_t operator()(const char *str) const { return hash_type{}(str); }
        std::size_t operator()(std::string_view str) const { return hash_type{}(str); }
        std::size_t operator()(std::string const &str) const { return hash_type{}(str); }
    };

    static const std::unordered_map<std::string, token_type, string_hash, std::equal_to<>> keywords;

    std::string input;  // The user's input
    std::string::iterator ch;  // The byte to evaluate

    void read_char() noexcept;
    char peek() noexcept;
    void skip_whitespace() noexcept;
    std::string_view read_ident() noexcept;
    std::string_view read_int() noexcept;
public:
    lexer(std::string &&input_);
    lexer(lexer &&) = default;

    void next_token(token &) noexcept;
};
