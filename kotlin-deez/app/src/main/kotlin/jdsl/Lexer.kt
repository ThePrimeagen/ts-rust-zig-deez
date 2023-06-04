package jdsl

import jdsl.TokenType.*

class Lexer(
    private val source: String
) {

    private val keywords = hashMapOf(
        "fn" to FUNCTION,
        "let" to LET,
        "true" to TRUE,
        "false" to FALSE,
        "if" to IF,
        "else" to ELSE,
        "return" to RETURN,
    )

    private val tokens: MutableList<Token> = mutableListOf()
    private var start = 0
    private var current = 0

    private val isAtEnd get() = current >= source.length
    fun readTokens(): List<Token> {
        while (!isAtEnd) {
            start = current
            readToken()
        }
        addToken(EOF, "")
        return tokens
    }

    private fun readToken() {
        when (val char = peek()) {
            '(', ')', '{', '}', ',', ';', '+', '/', '*', '-', ' ', '\n', '\r', '\t', '\u00A0' -> singleCharToken(char)
            '!', '=', '<', '>' -> twoCharToken(char)
            '"' -> string()?.let { addToken(STRING, it) }
            else -> {
                when {
                    char.isDigit() -> addToken(NUMBER, number())
                    char.isAlpha() -> addToken(identifier())
                    else -> addToken(ILLEGAL)
                }
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
        '*' -> STAR
        '-' -> MINUS
        '/' -> SLASH
        ' ', '\r', '\t', '\u00A0' -> null
        else -> null
    }.also {
        advance()
    }?.let {
        addToken(it, char.toString())
    }

    private fun twoCharToken(char: Char) = when (char) {
        '!' -> if (peekNext() == '=') NOT_EQ else BANG
        '=' -> if (peekNext() == '=') EQ else ASSIGN
        '<' -> if (peekNext() == '=') LE else LESS
        '>' -> if (peekNext() == '=') GE else GREATER
        else -> null
    }?.let {
        when (it) {
            NOT_EQ, EQ, LE, GE -> {
                advance()
                advance()
            }

            BANG, ASSIGN, LESS, GREATER -> advance()

            else -> ILLEGAL
        }
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
        val text = source.substring(start, current)
        val token = Token(type, literal ?: text)
        tokens += token
    }
    private fun peek() = if (!isAtEnd) source[current] else '\u0000'
    private fun peekNext() = if (current + 1 < source.length) source[current + 1] else '\u0000'
    private fun Char.isAlpha() = (this in 'a'..'z') || (this in 'A'..'Z') || this == '_'
    private fun Char.isDigit() = (this in '0'..'9')
    private fun Char.isAlphaNumeric() = isDigit() || isAlpha()
}