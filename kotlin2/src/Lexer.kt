package dev.hermannm.monkeylang

import kotlin.text.isWhitespace

class Lexer(private val input: String) : Iterator<Token> {
    private var currentIndex: Int = 0

    override fun next(): Token {
        skipWhitespace()

        val token = when (val character = getCurrentCharacter()) {
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
            '{' -> Token.LeftSquirly
            '}' -> Token.RightSquirly
            '[' -> Token.LeftBracket
            ']' -> Token.RightBracket
            '=' -> if (consumeNextIfMatching('=')) Token.Equals else Token.Assign
            '!' -> if (consumeNextIfMatching('=')) Token.NotEquals else Token.Bang
            in 'a'..'z', in 'A'..'Z', '_' -> return readKeywordOrIdentifier()
            in '0'..'9' -> return readInteger()
            '"' -> return readString()
            null -> Token.EndOfFile
            else -> Token.Illegal(character.toString())
        }

        currentIndex++
        return token
    }

    override fun hasNext(): Boolean {
        // <= instead of <, to include final Token.EndOfFile
        return currentIndex <= input.length
    }

    private fun getCurrentCharacter(): Char? = input.getOrNull(currentIndex)

    private fun consumeNextIfMatching(characterToMatch: Char): Boolean {
        val matching = input.getOrNull(currentIndex + 1) == characterToMatch
        if (matching) {
            currentIndex++
        }
        return matching
    }

    private fun readKeywordOrIdentifier(): Token {
        val startIndex = currentIndex

        while (true) {
            when (getCurrentCharacter()) {
                in 'a'..'z', in 'A'..'Z', '_' -> { currentIndex++ }
                else -> break
            }
        }

        return when (val word = input.substring(startIndex, currentIndex)) {
            "fn" -> Token.Function
            "let" -> Token.Let
            "if" -> Token.If
            "else" -> Token.Else
            "return" -> Token.Return
            "true" -> Token.True
            "false" -> Token.False
            else -> Token.Identifier(word)
        }
    }

    private fun readInteger(): Token {
        val startIndex = currentIndex
        while (getCurrentCharacter() in '0'..'9') {
            currentIndex++
        }

        val string = input.substring(startIndex, currentIndex)

        return try {
            Token.Integer(string.toInt())
        } catch (_: NumberFormatException) {
            Token.Illegal(string)
        }
    }

    private fun readString(): Token {
        val startIndex = currentIndex + 1

        while (true) {
            currentIndex++
            when (getCurrentCharacter()) {
                '"' -> break
                null -> return Token.Illegal(input.substring(startIndex - 1, currentIndex))
            }
        }

        val string = input.substring(startIndex, currentIndex)

        // Skip closing quote
        currentIndex++

        return Token.StringLiteral(string)
    }

    private fun skipWhitespace() {
        while (getCurrentCharacter()?.isWhitespace() == true) {
            currentIndex++
        }
    }
}
