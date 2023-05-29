package dev.hermannm.monkeylang

sealed class Token {
    // Tokens with data
    data class Identifier(val name: String) : Token()
    data class Integer(val value: Int) : Token()
    data class StringLiteral(val literal: String) : Token()

    // Keywords
    object Function : Token()
    object Let : Token()
    object If : Token()
    object Else : Token()
    object Return : Token()
    object True : Token()
    object False : Token()

    // Single-character tokens
    object Assign : Token()
    object Plus : Token()
    object Minus : Token()
    object Slash : Token()
    object Asterisk : Token()
    object Bang : Token()
    object LessThan : Token()
    object GreaterThan : Token()
    object Comma : Token()
    object Semicolon : Token()
    object Colon : Token()
    object LeftParen : Token()
    object RightParen : Token()
    object LeftBrace : Token()
    object RightBrace : Token()
    object LeftBracket : Token()
    object RightBracket : Token()

    // Double-character tokens
    object Equals : Token()
    object NotEquals : Token()

    object EndOfFile : Token()
    object Illegal : Token()
}
