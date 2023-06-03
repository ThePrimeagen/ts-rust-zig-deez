module main

import tokens

pub keywords := {
    'fn': tokens.Token{type: tokens.TokenType.Function, literal: 'fn'},
    'let': tokens.Token{type: tokens.TokenType.Let, literal: 'let'}
}

fn is_letter(ch rune) bool {
    return ch.is_alpha() or ch == '_'
}

pub struct Lexer {
    mut input string
    input_length int
    mut position int
    mut read_position int
    mut ch rune
}

fn (mut lexer Lexer) read_char() {
    if lexer.read_position >= lexer.input_length {
        lexer.ch = '\0'
    } else {
        lexer.ch = lexer.input[lexer.read_position]
    }
    lexer.position = lexer.read_position
    lexer.read_position += 1
}

fn (mut lexer Lexer) get_next_token() tokens.Token {
    lexer.skip_whitespace()

    tok_type := tokens.char_to_token_type[lexer.ch]
    if tok_type != tokens.TokenType.Illegal {
        tok := tokens.Token{type: tok_type, literal: tok_type}
        lexer.read_char()
        return tok
    }

    if lexer.is_letter(lexer.ch) {
        indent := lexer.read_ident()
        return keywords[indent, default: tokens.Token{type: tokens.TokenType.Ident, literal: indent}]
    }

    if lexer.ch.is_digit() {
        return tokens.Token{type: tokens.TokenType.Int, literal: lexer.read_int()}
    }

    return tokens.Token{type: tokens.TokenType.Illegal, literal: lexer.ch}
}

fn (mut lexer Lexer) read_int() string {
    pos := lexer.position

    for lexer.ch.is_digit() {
        lexer.read_char()
    }

    return lexer.input[pos..lexer.position]
}

fn (mut lexer Lexer) read_ident() string {
    pos := lexer.position

    for lexer.is_letter(lexer.ch) {
        lexer.read_char()
    }

    return lexer.input[pos..lexer.position]
}

fn (mut lexer Lexer) skip_whitespace() {
    while lexer.ch.is_space() {
        lexer.read_char()
    }
}
