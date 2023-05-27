object Lexer {
    fun parseToken(value: String) : Sequence<Token>  = sequence {
        val tokenizer = Tokenizer(value)
        while(tokenizer.hasNext()) {
            val next = tokenizer.peek()
            when {
                next.isLetter() ->
                    yield(tokenizer.nextUntil { !it.isLetterOrDigit() && !it.isUnderscore() }.asToken())
                next.isDigit() ->
                    yield(Token(type = TokenType.INT, value = tokenizer.nextUntil { !it.isDigit() }))
                next.isDoubleQuote() ->
                    yield(Token(type = TokenType.STRING, value = tokenizer.nextTo(true) { it.isDoubleQuote() }))
                else -> yield(tokenizer.next().asToken())
            }
        }

        yield(Token(type = TokenType.EOF))
    }
}

class Tokenizer(value: String) {
    private var position = 0
    private val value = value.trim()

    fun peek() : Char {
        skipWhitespace()
        return value[position]
    }

    fun hasNext() = position < value.length

    fun next(): Char {
        skipWhitespace()
        return value[position++]
    }

    fun nextUntil(skipCurrent: Boolean = false, predicate: (Char) -> Boolean): String {
        skipWhitespace()
        if (skipCurrent) position++
        val oldPosition = position
        while (position < value.length && !predicate(value[position])) position++
        return value.substring(oldPosition, position)
    }

    fun nextTo(skipCurrent: Boolean = false, predicate: (Char) -> Boolean): String {
        val result = nextUntil(skipCurrent, predicate)
        position++
        return result
    }

    private fun skipWhitespace() {
        while(value[position].isWhitespace()) position++
    }
}

enum class TokenType { COMMA, EOF, EQUAL, FUNC, IDENT, ILLEGAL, INT, LET, LPAREN, LSQUIRLY, PLUS, RPAREN, RSQUIRLY, SEMI, STRING }

data class Token(val type: TokenType, val value: String? = null)


fun Char.isDoubleQuote() = this == '"'
fun Char.isUnderscore() = this == '_'

fun String.asToken() = when(this.toLowerCase()) {
    "let" -> Token(type = TokenType.LET, value = this)
    "fn" -> Token(type = TokenType.FUNC, value = this)
    else -> Token(type = TokenType.IDENT, value = this)
}

fun Char.asToken() = when(this) {
    '=' -> Token(type = TokenType.EQUAL, value = this.toString())
    '+' -> Token(type = TokenType.PLUS, value = this.toString())
    ',' -> Token(type = TokenType.COMMA, value = this.toString())
    ';' -> Token(type = TokenType.SEMI, value = this.toString())
    '(' -> Token(type = TokenType.LPAREN, value = this.toString())
    ')' -> Token(type = TokenType.RPAREN, value = this.toString())
    '{' -> Token(type = TokenType.LSQUIRLY, value = this.toString())
    '}' -> Token(type = TokenType.RSQUIRLY, value = this.toString())
    else -> Token(type = TokenType.ILLEGAL, value = this.toString())
}
