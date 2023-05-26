package dev.hermannm.monkeylang

import kotlin.text.isWhitespace

sealed class Token {
    data class Identifier(val name: String) : Token()
    data class Integer(val value: String) : Token()
    object Let : Token()
    object Function : Token()
    object Equals : Token()
    object Plus : Token()
    object Comma : Token()
    object Semicolon : Token()
    object LeftParen : Token()
    object RightParen : Token()
    object LeftBrace : Token()
    object RightBrace : Token()
    object EndOfFile : Token()
    object Illegal : Token()
}

class Lexer(private val input: String) {
    private var position: Int = 0
    private var readPosition: Int = 0
    private var currentCharacter: Char = Char.MIN_VALUE

    init {
        readCharacter()
    }

    fun nextToken(): Token {
        skipWhitespace()

        val token = when (currentCharacter) {
            '=' -> Token.Equals
            '+' -> Token.Plus
            ',' -> Token.Comma
            ';' -> Token.Semicolon
            '(' -> Token.LeftParen
            ')' -> Token.RightParen
            '{' -> Token.LeftBrace
            '}' -> Token.RightBrace
            Char.MIN_VALUE -> Token.EndOfFile
            in 'a'..'z', in 'A'..'Z', '_' -> {
                return when (val identifier = readIdentifier()) {
                    "fn" -> Token.Function
                    "let" -> Token.Let
                    else -> Token.Identifier(identifier)
                }
            }
            in '0'..'9' -> {
                val integer = readInteger()
                return Token.Integer(integer)
            }
            else -> Token.Illegal
        }

        readCharacter()
        return token
    }

    private fun readCharacter() {
        currentCharacter = if (readPosition >= input.length) {
            Char.MIN_VALUE
        } else {
            input[readPosition]
        }

        position = readPosition
        readPosition++
    }

    private fun readIdentifier(): String {
        val startPosition = position
        while (currentCharacter.isLetter() || currentCharacter == '_') {
            readCharacter()
        }
        return input.substring(startPosition, position)
    }

    private fun readInteger(): String {
        val startPosition = position
        while (currentCharacter.isDigit()) {
            readCharacter()
        }
        return input.substring(startPosition, position)
    }

    private fun skipWhitespace() {
        while (currentCharacter.isWhitespace()) {
            readCharacter()
        }
    }
}
