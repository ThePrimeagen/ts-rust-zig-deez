package dev.hermannm.monkeylang

import kotlin.text.isWhitespace

class Lexer(private val input: String) : Iterator<Token> {
    private var currentIndex: Int = 0

    override fun next(): Token {
        skipWhitespace()
        return when (peek()) {
            '+' -> Token(TokenType.Plus, consume())
            '-' -> Token(TokenType.Minus, consume())
            '/' -> Token(TokenType.Slash, consume())
            '*' -> Token(TokenType.Asterisk, consume())
            '<' -> Token(TokenType.LessThan, consume())
            '>' -> Token(TokenType.GreaterThan, consume())
            ',' -> Token(TokenType.Comma, consume())
            ';' -> Token(TokenType.Semicolon, consume())
            ':' -> Token(TokenType.Colon, consume())
            '(' -> Token(TokenType.LeftParen, consume())
            ')' -> Token(TokenType.RightParen, consume())
            '{' -> Token(TokenType.LeftSquirly, consume())
            '}' -> Token(TokenType.RightSquirly, consume())
            '[' -> Token(TokenType.LeftBracket, consume())
            ']' -> Token(TokenType.RightBracket, consume())
            '=' -> when (peek(1)) {
                '=' -> Token(TokenType.Equals, consume(2))
                else -> Token(TokenType.Assign, consume())
            }
            '!' -> when (peek(1)) {
                '=' -> Token(TokenType.NotEquals, consume(2))
                else -> Token(TokenType.Bang, consume())
            }
            in 'a'..'z', in 'A'..'Z', '_' -> consumeKeywordOrIdentifier()
            in '0'..'9' -> consumeInteger()
            '"' -> return consumeStringLiteral()
            null -> Token(TokenType.EndOfFile, "")
            else -> Token(TokenType.Illegal, consume())
        }
    }

    override fun hasNext(): Boolean {
        // <= instead of <, to include final Token.EndOfFile
        return currentIndex <= input.length
    }

    private fun consume(amount: Int = 1): String = buildString {
        repeat(amount) {
            if (currentIndex >= input.length) {
                error("Tried to consume past the end of input.")
            }
            append(input[currentIndex++])
        }
    }

    private fun consumeWhile(predicate: (char: Char?) -> Boolean) = buildString {
        while (predicate(peek())) {
            append(consume())
        }
    }

    private fun peek(forward: Int = 0): Char? = input.getOrNull(currentIndex + forward)

    private fun consumeKeywordOrIdentifier(): Token {
        fun isIdentifier(char: Char) = char.isLetterOrDigit() || char == '_'
        val identifier = consumeWhile { char -> char != null && isIdentifier(char) }
        val type = lookupIdentifier(identifier)
        return Token(type, identifier)
    }

    private fun consumeInteger(): Token {
        val literal = consumeWhile { char -> char != null && char.isDigit() }
        return Token(TokenType.Integer, literal)
    }

    private fun consumeStringLiteral(): Token {
        var stringLiteral = buildString {
            append(consume()) // consume the initial quote
            append(consumeWhile { char -> char != null && char != '"' })
        }
        return try {
            stringLiteral += consume() // consume the closing quote
            Token(TokenType.StringLiteral, stringLiteral)
        } catch (e: Exception) {
            Token(TokenType.Illegal, stringLiteral)
        }
    }

    private fun skipWhitespace() {
        while (peek()?.isWhitespace() == true) {
            consume()
        }
    }
}
