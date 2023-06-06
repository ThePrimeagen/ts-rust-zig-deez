package monkeylang

import kotlin.text.isWhitespace

class Lexer(private val input: String) : Iterator<Token> {
    private var currentIndex: Int = 0
    private var line = 1
    private var column = 0
    private var isFinished = false

    override fun next(): Token {
        skipWhitespace()
        return when (peek()) {
            '+' -> consumeToken(TokenType.Plus)
            '-' -> consumeToken(TokenType.Minus)
            '/' -> consumeToken(TokenType.Slash)
            '*' -> consumeToken(TokenType.Asterisk)
            '<' -> consumeToken(TokenType.LessThan)
            '>' -> consumeToken(TokenType.GreaterThan)
            ',' -> consumeToken(TokenType.Comma)
            ';' -> consumeToken(TokenType.Semicolon)
            ':' -> consumeToken(TokenType.Colon)
            '(' -> consumeToken(TokenType.LeftParen)
            ')' -> consumeToken(TokenType.RightParen)
            '{' -> consumeToken(TokenType.LeftSquirly)
            '}' -> consumeToken(TokenType.RightSquirly)
            '[' -> consumeToken(TokenType.LeftBracket)
            ']' -> consumeToken(TokenType.RightBracket)
            '=' -> when (peek(1)) {
                '=' -> consumeToken(TokenType.Equals, 2)
                else -> consumeToken(TokenType.Assign)
            }
            '!' -> when (peek(1)) {
                '=' -> consumeToken(TokenType.NotEquals, 2)
                else -> consumeToken(TokenType.Bang)
            }
            in 'a'..'z', in 'A'..'Z', '_' -> consumeKeywordOrIdentifier()
            in '0'..'9' -> consumeInteger()
            '"' -> return consumeStringLiteral()
            null -> {
                isFinished = true
                Token(TokenType.EndOfFile, "", capturePositionAsRange())
            }
            else -> consumeToken(TokenType.Illegal)
        }
    }

    override fun hasNext(): Boolean = !isFinished

    private fun capturePosition(): DocumentPosition = DocumentPosition(line, column)

    private fun capturePositionAsRange(): DocumentRange {
        val position = capturePosition()
        return position..position
    }

    private fun captureRange(block: () -> String): Pair<String, DocumentRange> {
        val startPosition = capturePosition()
        val result = block()
        val endPosition = capturePosition()
        return result to DocumentRange(startPosition, endPosition)
    }

    private fun consume(amount: Int = 1): String = buildString {
        repeat(amount) {
            if (currentIndex >= input.length) {
                error("Tried to consume past the end of input.")
            }
            val char = input[currentIndex++]
            column++
            if (char == '\n') {
                line++
                column = 0
            }
            append(char)
        }
    }

    private fun consumeWhile(predicate: (char: Char?) -> Boolean) = buildString {
        while (predicate(peek())) {
            append(consume())
        }
    }

    private fun consumeToken(type: TokenType, amount: Int = 1): Token {
        val literal = captureRange {
            consume(amount)
        }
        return Token(type, literal.first, literal.second)
    }

    private fun peek(forward: Int = 0): Char? = input.getOrNull(currentIndex + forward)

    private fun consumeKeywordOrIdentifier(): Token {
        fun isIdentifier(char: Char) = char.isLetterOrDigit() || char == '_'
        val identifier = captureRange {
            consumeWhile { char -> char != null && isIdentifier(char) }
        }
        val type = lookupIdentifier(identifier.first)
        return Token(type, identifier.first, identifier.second)
    }

    private fun consumeInteger(): Token {
        val literal = captureRange {
            consumeWhile { char -> char != null && char.isDigit() }
        }
        return Token(TokenType.Integer, literal.first, literal.second)
    }

    private fun consumeStringLiteral(): Token {
        val startPosition = capturePosition()
        var stringLiteral = buildString {
            append(consume()) // consume the initial quote
            append(consumeWhile { char -> char != null && char != '"' })
        }
        return try {
            stringLiteral += consume() // consume the closing quote
            val endPosition = capturePosition()
            Token(TokenType.StringLiteral, stringLiteral, startPosition..endPosition)
        } catch (e: Exception) {
            val endPosition = capturePosition()
            Token(TokenType.Illegal, stringLiteral, startPosition..endPosition)
        }
    }

    private fun skipWhitespace() {
        while (peek()?.isWhitespace() == true) {
            consume()
        }
    }
}
