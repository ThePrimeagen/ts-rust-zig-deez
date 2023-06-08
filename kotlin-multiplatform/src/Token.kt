package monkeylang

enum class TokenType {
    Illegal,
    EndOfFile,

    // Identifiers + literals
    Identifier,
    Integer,
    StringLiteral,

    // Operators
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,

    // Comparison
    Equals,
    NotEquals,
    Bang,
    LessThan,
    GreaterThan,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,

    // Delimiters
    Semicolon,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftSquirly,
    RightSquirly,
    LeftBracket,
    RightBracket,
}

data class Token(val type: TokenType, val literal: String, val range: DocumentRange)

fun lookupIdentifier(identifier: String): TokenType {
    return when (identifier) {
        "fn" -> TokenType.Function
        "let" -> TokenType.Let
        "if" -> TokenType.If
        "else" -> TokenType.Else
        "return" -> TokenType.Return
        "true" -> TokenType.True
        "false" -> TokenType.False
        else -> TokenType.Identifier
    }
}
