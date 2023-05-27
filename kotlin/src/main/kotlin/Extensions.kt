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
