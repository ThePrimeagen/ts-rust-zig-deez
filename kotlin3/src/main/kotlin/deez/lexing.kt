package deez

enum class TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENTIFIER,
    INTEGER,

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,

    // Keywords
    FUNCTION,
    LET,
}

data class Token(val type: TokenType, val literal: String)

fun lexer(input: String) = sequence<Token> {
    var position = 0

    fun peekChar() = if (position >= input.length) null else input[position]

    fun consumeChar(): String = input[position++].toString()

    fun consumeIdentifier() = buildString {
        while (true) {
            val char = peekChar()
            if (char == null || !char.isLetterOrDigit()) break
            append(consumeChar())
        }
    }

    fun consumeInteger() = buildString {
        while (true) {
            val char = peekChar()
            if (char == null || !char.isDigit()) break
            append(consumeChar())
        }
    }

    fun lookupIdentifier(identifier: String) = when (identifier) {
        "fn" -> TokenType.FUNCTION
        "let" -> TokenType.LET
        else -> TokenType.IDENTIFIER
    }

    while (position < input.length) {
        val currentChar = peekChar() ?: break
        when (currentChar) {
            ' ', '\t', '\n', '\r' -> consumeChar()
            '=' -> yield(Token(TokenType.ASSIGN, consumeChar()))
            ';' -> yield(Token(TokenType.SEMICOLON, consumeChar()))
            '(' -> yield(Token(TokenType.LEFT_PAREN, consumeChar()))
            ')' -> yield(Token(TokenType.RIGHT_PAREN, consumeChar()))
            ',' -> yield(Token(TokenType.COMMA, consumeChar()))
            '+' -> yield(Token(TokenType.PLUS, consumeChar()))
            '{' -> yield(Token(TokenType.LEFT_BRACE, consumeChar()))
            '}' -> yield(Token(TokenType.RIGHT_BRACE, consumeChar()))
            else -> when {
                currentChar.isLetter() -> {
                    val identifier = consumeIdentifier()
                    val type = lookupIdentifier(identifier)
                    yield(Token(type, identifier))
                }
                currentChar.isDigit() -> {
                    val integer = consumeInteger()
                    yield(Token(TokenType.INTEGER, integer))
                }
                else -> yield(Token(TokenType.ILLEGAL, consumeChar()))
            }
        }
    }
    yield(Token(TokenType.EOF, ""))
}