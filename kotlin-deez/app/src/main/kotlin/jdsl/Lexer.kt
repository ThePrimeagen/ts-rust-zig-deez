package jdsl

import jdsl.TokenType.*

class Lexer(
    private val source: String
) {

    private val keywords = hashMapOf(
        "else" to ELSE,
        "fun" to FUNCTION,
        "if" to IF,
        "return" to RETURN,
        "let" to LET,
    )

    private val tokens: MutableList<Token> = mutableListOf()
    private var start = 0
    private var current = 0

    private val isAtEnd get() = current >= source.length
    fun readTokens(): List<Token> {
        while (!isAtEnd) { // We are at the beginning of the next lexeme.
            start = current
            readToken()
        }
        addToken(EOF)
        return tokens
    }

    private fun readToken() {
        when (val char = peek()) {
            '(', ')', '{', '}', ',', ';', '+', '=', ' ', '\n', '\r', '\t', '\u00A0' -> singleCharToken(char)
            '"' -> string()?.let { addToken(STRING, it) }
            in '0'..'9' -> addToken(NUMBER, number())
            in 'a'..'z', in 'A'..'Z', '_' -> addToken(identifier())
            else -> {
                addToken(ILLEGAL)
            }
        }
    }

    private fun identifier(): TokenType {
        while (peek().isAlphaNumeric()) {
            advance()
        }

        return keywords[source.substring(start, current)] ?: IDENTIFIER
    }


    private fun singleCharToken(char: Char) = when (char) {
        '(' -> LPAREN
        ')' -> RPAREN
        '{' -> LSQUIRLY
        '}' -> RSQUIRLY
        ',' -> COMMA
        ';' -> SEMICOLON
        '+' -> PLUS
        '=' -> EQUAL
        ' ', '\r', '\t', '\u00A0' -> null
        else -> null
    }.also {
        advance()
    }?.let {
        addToken(it)
    }

    private fun number(): String {
        while (peek().isDigit() || peek() == '.' && peekNext().isDigit()) {
            advance()
        }
        return source.substring(start, current)
    }

    private fun advance() = source[current].also {
        current++
    }

    private fun string(): String? {
        do {
            advance()
        } while (peek() != '"' && !isAtEnd)

        return if (isAtEnd) {
            addToken(ILLEGAL)
            null
        } else {
            advance()
            source.substring(start + 1, current - 1)
        }
    }

    private fun addToken(type: TokenType) = addToken(type, null)

    private fun addToken(type: TokenType, literal: String?) {
        tokens += Token(type, literal)
    }

    private fun peek() = if (!isAtEnd) source[current] else '\u0000'
    private fun peekNext() = if (current + 1 < source.length) source[current + 1] else '\u0000'
    private fun Char.isAlpha() = (this in 'a'..'z') || (this in 'A'..'Z') || this == '_'
    private fun Char.isDigit() = (this in '0'..'9')
    private fun Char.isAlphaNumeric() = isDigit() || isAlpha()
}