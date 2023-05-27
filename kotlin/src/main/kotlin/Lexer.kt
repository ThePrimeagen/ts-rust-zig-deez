enum class TokenType { COMMA, EOF, EQUAL, FUNC, IDENT, ILLEGAL, INT, LET, LPAREN, LSQUIRLY, PLUS, RPAREN, RSQUIRLY, SEMI, STRING }
data class Token(val type: TokenType, val value: String? = null)

object Lexer {
    fun parse(value: String) : Sequence<Token>  = sequence {
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
