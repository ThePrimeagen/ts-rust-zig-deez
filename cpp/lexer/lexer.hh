#pragma once


#include <string>
#include <string_view>
#include <variant>

#include <cstdint>


enum class token_type {
    illegal,
    eof,
    identifier,
    integer,
    assign,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lsquirly,
    rsquirly,
    function,
    let
};


struct token final {
    token_type type;
    std::variant<std::string, char> literal;
};


class lexer final {
  private:
    std::string_view input_;  // The user's input
    size_t position_;  // Point to the current byte
    size_t read_position_;  // Peeks at the next byte
    char byte_;  // The byte to evaluate

  public:
    lexer(std::string_view input);
    void read_char(void) noexcept;
    void next_token(token &) noexcept;
};
