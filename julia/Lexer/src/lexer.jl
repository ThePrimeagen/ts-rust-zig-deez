module Lexer

include("tokens.jl")
using .Tokens

is_letter(ch) = isletter(ch) || ch == '_'

mutable struct LexerState
    input::String
    position::Int
    read_position::Int
    ch::Char
end

function LexerState(input::String)
    lexer = LexerState(input, 0, 0, '\0')
    read_char(lexer)
    return lexer
end

function read_char(lexer::LexerState)
    if lexer.read_position >= length(lexer.input)
        lexer.ch = '\0'
    else
        lexer.ch = lexer.input[lexer.read_position+1]
    end
    lexer.position = lexer.read_position
    lexer.read_position += 1
end

function next_token(lexer::LexerState)
    # Skip whitespace
    while lexer.ch == ' ' || lexer.ch == '\t' || lexer.ch == '\n' || lexer.ch == '\r'
        read_char(lexer)
    end

    # Analyze the current character and return the appropriate token
    tok = Token(ILLEGAL(), "")
    if lexer.ch == '='
        tok = Token(ASSIGN(), string(lexer.ch))
    elseif lexer.ch == '+'
        tok = Token(PLUS(), string(lexer.ch))
    elseif lexer.ch == ','
        tok = Token(COMMA(), string(lexer.ch))
    elseif lexer.ch == ';'
        tok = Token(SEMICOLON(), string(lexer.ch))
    elseif lexer.ch == '('
        tok = Token(LPAREN(), string(lexer.ch))
    elseif lexer.ch == ')'
        tok = Token(RPAREN(), string(lexer.ch))
    elseif lexer.ch == '{'
        tok = Token(LBRACE(), string(lexer.ch))
    elseif lexer.ch == '}'
        tok = Token(RBRACE(), string(lexer.ch))
    elseif is_letter(lexer.ch)
        literal = read_identifier(lexer)
        tok = Token(IDENT(), literal)
    elseif isdigit(lexer.ch)
        literal = read_number(lexer)
        tok = Token(INT(), literal)
    elseif lexer.ch == '\0'
        tok = Token(EOF(), "")
    else
        tok = Token(ILLEGAL(), string(lexer.ch))
    end

    read_char(lexer)
    return tok
end

function read_identifier(lexer::LexerState)
    start = lexer.position
    while is_letter(lexer.ch)
        read_char(lexer)
    end
    return lexer.input[start+1 : lexer.position]
end

function read_number(lexer::LexerState)
    start = lexer.position
    while isdigit(lexer.ch)
        read_char(lexer)
    end
    return lexer.input[start+1 : lexer.position]
end

export LexerState, next_token

end # module Lexer
