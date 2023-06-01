package dev.hermannm.monkeylang

sealed class Token {
    // Tokens with data
    data class Identifier(val name: String) : Token()
    data class Integer(val value: Int) : Token()
    data class StringLiteral(val value: String) : Token()
    data class Illegal(val source: String) : Token()

    // Keywords
    data object Function : Token()
    data object Let : Token()
    data object If : Token()
    data object Else : Token()
    data object Return : Token()
    data object True : Token()
    data object False : Token()

    // Single-character tokens
    data object Assign : Token()
    data object Plus : Token()
    data object Minus : Token()
    data object Slash : Token()
    data object Asterisk : Token()
    data object Bang : Token()
    data object LessThan : Token()
    data object GreaterThan : Token()
    data object Comma : Token()
    data object Semicolon : Token()
    data object Colon : Token()
    data object LeftParen : Token()
    data object RightParen : Token()
    data object LeftSquirly : Token()
    data object RightSquirly : Token()
    data object LeftBracket : Token()
    data object RightBracket : Token()

    // Double-character tokens
    data object Equals : Token()
    data object NotEquals : Token()

    data object EndOfFile : Token()
}
