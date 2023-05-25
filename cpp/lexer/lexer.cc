#include "lexer.hh"


#include <exception>


lexer::lexer(const std::string input)
  : input_{input},
    position_{0},
    read_position_{1},
    byte_{input[0]}
{
    if (input.empty()) {
        throw std::exception();
    }

    return;
}

void
lexer::read_char(void) noexcept
{
    if (this->read_position_ >= this->input_.length()) {
        this->byte_ = '\0';
    } else [[likely]] {
        this->byte_ = this->input_[this->read_position_];
    }

    this->position_ = this->read_position_;
    this->read_position_ += 1;

    return;
}

void
lexer::next_token(token &t) noexcept
{
    switch (this->byte_) {
      case '=':
        t.type = token_type::assign;
        t.literal = this->byte_;
        break;

      case ';':
        t.type = token_type::semicolon;
        t.literal = this->byte_;
        break;

      case '(':
        t.type = token_type::lparen;
        t.literal = this->byte_;
        break;

      case ')':
        t.type = token_type::rparen;
        t.literal = this->byte_;
        break;

      case ',':
        t.type = token_type::comma;
        t.literal = this->byte_;
        break;

      case '+':
        t.type = token_type::plus;
        t.literal = this->byte_;
        break;

      case '{':
        t.type = token_type::lsquirly;
        t.literal = this->byte_;
        break;

      case '}':
        t.type = token_type::rsquirly;
        t.literal = this->byte_;
        break;

      case '\0':
        t.type = token_type::eof;
        t.literal = this->byte_;
        break;

      default:
        t.type = token_type::illegal;
        t.literal = this->byte_;
    }

    this->read_char();

    return;
}
