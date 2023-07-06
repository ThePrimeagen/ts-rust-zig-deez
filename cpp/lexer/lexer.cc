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


static const std::unordered_map<token_type, std::string> token_names{

    {token_type::Identifier, "Identifier"},
    { token_type::Integer,"Integer" },
    { token_type::String,"String" },

    { token_type::Illegal,"Illegal" },
    { token_type::Eof,"Eof" },
    { token_type::Assign,"Assign" },

    { token_type::Bang,"Bang" },
    { token_type::Dash,"Dash" },
    { token_type::ForwardSlash,"ForwardSlash" },
    { token_type::Asterisk,"Asterisk" },
    { token_type::Equal, "Equal" },
    { token_type::NotEqual,"NotEqual" },
    { token_type::LessThan,"LessThan" },
    { token_type::GreaterThan,"GreaterThan" },

    { token_type::Plus,"Plus" },
    { token_type::Comma,"Comma" },
    { token_type::Semicolon,"Semicolon" },
    { token_type::Colon,"Colon" },
    { token_type::LParen,"LParen" },
    { token_type::RParen,"RParen" },
    { token_type::LSquirly,"LSquirly" },
    { token_type::RSquirly,"RSquirly" },
    { token_type::LBracket,"LBracket" },
    { token_type::RBracket,"RBracket" },
    { token_type::Function,"Function" },
    { token_type::Let,"Let" },
    { token_type::If,"If" },
    { token_type::Else,"Else" },
    { token_type::Return,"Return" },
    { token_type::True,"True" },
    { token_type::False,"False" }
};

std::string_view getTokenTypeName(token_type t)
{
    auto iter = token_names.find(t);
    if (iter == token_names.end()) return "??";
    else return iter->second;

}

std::string token::to_string() const {
    std::string result;
    result.reserve(40); // should be enough
    result = "{ ";
    result += getTokenTypeName(type);
    result += ", ";
    result += literal;
    result += "}";
    return result;
}

lexer::lexer(std::string &&input_)
  : input{std::move(input_)}
  , ch{input.begin()}
{}

void
lexer::read_char(void) noexcept
{
    if (ch != input.end()) [[likely]] {
        ch++;
    }
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
    while (ch != input.end() && (std::isalpha(*ch) || *ch == '_'))
    {
        read_char();
    }
    return std::string_view(pos, ch);
}

void
lexer::read_int(token &t) noexcept
{
    std::string::iterator pos = ch;
    if (peek() == 'x')
    {
        read_char();
        read_char();
        pos = ch;
        t.type = token_type::HexInteger;
        while (ch != input.end() && std::isxdigit(*ch))
            read_char();
    }
    else {
        t.type = token_type::Integer;
        while (ch != input.end() && std::isdigit(*ch))
            read_char();
    }
    t.literal = std::string_view(pos, ch);
}

std::string_view
lexer::read_string() noexcept
{
    read_char();
    const std::string::iterator pos = ch;
    while (ch != input.end() && *ch!='"')
    {
        read_char();
    }
    std::string_view result(pos, ch);
    read_char();
    return result;
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
    case '[':
        t.set(token_type::LBracket, ch);
        break;
    case ']':
        t.set(token_type::RBracket, ch);
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
    case ':':
        t.set(token_type::Colon, ch);
        break;
    case '&':
        t.set(token_type::Ampersand, ch);
        break;
    case '|':
        t.set(token_type::Pipe, ch);
        break;
    case '~':
        t.set(token_type::Tilde, ch);
        break;
    case '^':
        t.set(token_type::Hat, ch);
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
    case '"': 
        t.set(token_type::String, read_string());
        return;
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
        //t.set(token_type::Integer, read_int());
        read_int(t);
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
