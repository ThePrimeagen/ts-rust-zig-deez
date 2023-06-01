package dev.hermannm.monkeylang

import kotlin.text.isWhitespace

class Lexer(private val input: String) {
    private var currentIndex: Int = 0

    /** @throws IllegalStateException If the input is invalid. */
    fun nextToken(): Token {
        skipWhitespace()

        val token = when (getCurrentCharacter()) {
            '+' -> Token.Plus
            '-' -> Token.Minus
            '/' -> Token.Slash
            '*' -> Token.Asterisk
            '<' -> Token.LessThan
            '>' -> Token.GreaterThan
            ',' -> Token.Comma
            ';' -> Token.Semicolon
            ':' -> Token.Colon
            '(' -> Token.LeftParen
            ')' -> Token.RightParen
            '{' -> Token.LeftBrace
            '}' -> Token.RightBrace
            '[' -> Token.LeftBracket
            ']' -> Token.RightBracket
            '=' -> {
                if (getNextCharacter() == '=') {
                    currentIndex++
                    Token.Equals
                } else {
                    Token.Assign
                }
            }
            '!' -> {
                if (getNextCharacter() == '=') {
                    currentIndex++
                    Token.NotEquals
                } else {
                    Token.Bang
                }
            }
            in 'a'..'z', in 'A'..'Z', '_' -> {
                return when (val identifier = readIdentifier()) {
                    "fn" -> Token.Function
                    "let" -> Token.Let
                    "if" -> Token.If
                    "else" -> Token.Else
                    "return" -> Token.Return
                    "true" -> Token.True
                    "false" -> Token.False
                    else -> Token.Identifier(identifier)
                }
            }
            in '0'..'9' -> {
                val integer = readInteger()
                return Token.Integer(integer)
            }
            '"' -> {
                val string = readString()
                return Token.StringLiteral(string)
            }
            null -> Token.EndOfFile
            else -> Token.Illegal
        }

        currentIndex++
        return token
    }

    private fun getCurrentCharacter(): Char? {
        return input.getOrNull(currentIndex)
    }

    private fun getNextCharacter(): Char? {
        return input.getOrNull(currentIndex + 1)
    }

    private fun readIdentifier(): String {
        val startIndex = currentIndex

        while (true) {
            val currentCharacter = getCurrentCharacter() ?: break

            if (currentCharacter.isLetter() || currentCharacter == '_') {
                currentIndex++
            } else {
                break
            }
        }

        return input.substring(startIndex, currentIndex)
    }

    /** @throws IllegalStateException If parsing integer from current position in input failed. */
    private fun readInteger(): Int {
        val startIndex = currentIndex
        while (getCurrentCharacter()?.isDigit() == true) {
            currentIndex++
        }

        val string = input.substring(startIndex, currentIndex)

        return try { string.toInt() } catch (err: Exception) {
            throw IllegalStateException("Failed to read integer '$string' from input", err)
        }
    }

    /** @throws IllegalStateException If input ended before closing quote for the string. */
    private fun readString(): String {
        val startPosition = currentIndex + 1

        while (true) {
            currentIndex++
            when (getCurrentCharacter()) {
                '"' -> break
                Char.MIN_VALUE -> throw IllegalStateException(
                    "Input ended before closing quote of string",
                )
            }
        }

        val string = input.substring(startPosition, currentIndex)

        // Skip closing quote
        currentIndex++

        return string
    }

    private fun skipWhitespace() {
        while (getCurrentCharacter()?.isWhitespace() == true) {
            currentIndex++
        }
    }
}
