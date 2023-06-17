#include "lexer.hh"
#include <cctype>


const std::unordered_map<std::string, token_type, lexer::string_hash, std::equal_to<>> lexer::keywords {
       {"fn",token_type::Function},
       {"let",token_type::Let},
       {"if",token_type::If},
       {"else",token_type::Else},
       {"return",token_type::Return},
       {"true",token_type::True},
       {"false",token_type::False}
};


lexer::lexer(std::string &&input_)
  : input{std::move(input_)}
  , ch{input.begin()}
{}

void
lexer::read_char(void) noexcept
{
    if (ch == input.end()) {
        return;
    }
    else [[likely]] {
     ch++;
    }
    return;
}

char
lexer::peek(void) noexcept
{
    if (ch + 1 == input.end()) {
        return '\0';
    }
    else [[likely]] {
     return *(ch + 1);
    }
}

void 
lexer::skip_whitespace() noexcept
{
    while (ch!=input.end() && std::isspace(*ch)) read_char();
}

std::string_view
lexer::read_ident() noexcept
{
    const std::string::iterator pos = ch;
    while (std::isalpha(*ch) || *ch == '_')
    {
        read_char();
    }
    return std::string_view(pos, ch);
}

std::string_view
lexer::read_int() noexcept
{
    const std::string::iterator pos = ch;
    while (std::isdigit(*ch))
    {
        read_char();
    }
    return std::string_view(pos, ch);
}

void
lexer::next_token(token &t) noexcept
{
    skip_whitespace();
    if (ch == input.end() || *ch == '\0')
    {
        t.set(token_type::Eof, "\0");
        return;
    }
    switch (*ch) {
    case '{':
        t.set(token_type::LSquirly, ch);
        break;
    case '}':
        t.set(token_type::RSquirly, ch);
        break;
    case '(':
        t.set(token_type::LParen, ch);
        break;
    case ')':
        t.set(token_type::RParen, ch);
        break;
    case ',':
        t.set(token_type::Comma, ch);
        break;
    case ';':
        t.set(token_type::Semicolon, ch);
        break;
    case '+':
        t.set(token_type::Plus, ch);
        break;
    case '-':
        t.set(token_type::Dash, ch);
        break;
    case '!':
        if (peek() == '=')
        {
            read_char();
            t.set(token_type::NotEqual, "!=");
        }
        else
            t.set(token_type::Bang, ch);
        break;

    case '>':
        t.set(token_type::GreaterThan, ch);
        break;
    case '<':
        t.set(token_type::LessThan, ch);
        break;
    case '*':
        t.set(token_type::Asterisk, ch);
        break;
    case '/':
        t.set(token_type::ForwardSlash, ch);
        break;
    case '=':
        if (peek() == '=')
        {
            read_char();
            t.set(token_type::Equal, "==");
        }
        else
            t.set(token_type::Assign, ch);
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        t.set(token_type::Integer, read_int());
        return;
    default:
        if (std::isalpha(*ch) || *ch == '_')
        {
            std::string_view word = read_ident();
            auto iter = keywords.find(word);
            if (iter != keywords.end())
                t.set(iter->second, word);
            else
                t.set(token_type::Identifier, word);
            return;
        }
        else
            t.set(token_type::Illegal, ch);
    }
    read_char();
    return;
}
