package deez

class Lexer(private val input: String) {

    private var pos: Int = 0

    private fun currentChar() = if (pos < input.length) input[pos] else Char.MIN_VALUE

    fun nextToken(): Token {
        skipWhitespace()

        return when (currentChar()) {
            '{' -> createToken(Token.Type.LEFT_BRACE)
            '}' -> createToken(Token.Type.RIGHT_BRACE)
            '(' -> createToken(Token.Type.LEFT_PARENTHESES)
            ')' -> createToken(Token.Type.RIGHT_PARENTHESES)
            ',' -> createToken(Token.Type.COMMA)
            ';' -> createToken(Token.Type.SEMICOLON)
            '+' -> createToken(Token.Type.PLUS)
            '=' -> createToken(Token.Type.EQUAL)
            in 'a'..'z', in 'A'..'Z', '_' -> readIdentifier()
            in '0'..'9' -> readNumber()
            Char.MIN_VALUE -> createToken(Token.Type.EOF)
            else -> createToken(Token.Type.ILLEGAL)
        }
    }

    private fun createToken(type: Token.Type): Token {
        pos++
        return Token(type)
    }

    private fun skipWhitespace() {
        while (currentChar().isWhitespace()) {
            pos++
        }
    }

    private fun readIdentifier(): Token {
        val start = pos
        while (currentChar().isLetter() || currentChar() == '_') {
            pos++
        }

        return when (val identifier = input.substring(start, pos)) {
            "let" -> Token(Token.Type.LET)
            "fn" -> Token(Token.Type.FUNCTION)
            else -> Token(Token.Type.IDENTIFIER, identifier)
        }
    }

    private fun readNumber(): Token {
        val start = pos
        while (currentChar().isDigit()) {
            pos++
        }
        return Token(Token.Type.NUMBER, input.substring(start, pos))
    }

    class Token(val type: Type, val identifier: String = "") {

        override fun toString() = "Token(type=$type, identifier='$identifier')"

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other !is Token) return false

            if (type != other.type) return false
            if (identifier != other.identifier) return false

            return true
        }

        override fun hashCode(): Int {
            var result = type.hashCode()
            result = 31 * result + identifier.hashCode()
            return result
        }

        enum class Type {
            LEFT_BRACE,
            RIGHT_BRACE,
            LEFT_PARENTHESES,
            RIGHT_PARENTHESES,
            COMMA,
            SEMICOLON,
            PLUS,
            EQUAL,
            IDENTIFIER,
            LET,
            FUNCTION,
            NUMBER,
            EOF,
            ILLEGAL
        }
    }
}