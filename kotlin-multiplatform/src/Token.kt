package dev.hermannm.monkeylang

sealed class Token {
    // Tokens with data
    data class Identifier(val name: String) : Token() {
        override fun toString(): String = "Identifier($name)"
    }
    data class Integer(val value: Int) : Token() {
        override fun toString(): String = "Integer($value)"
    }
    data class StringLiteral(val value: String) : Token() {
        override fun toString(): String = "StringLiteral(\"$value\")"
    }
    data class Illegal(val source: String) : Token() {
        override fun toString(): String = "Illegal($source)"
    }

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
    object LeftSquirly : Token()
    object RightSquirly : Token()
    object LeftBracket : Token()
    object RightBracket : Token()

    // Double-character tokens
    object Equals : Token()
    object NotEquals : Token()
    object EndOfFile : Token()

    override fun toString(): String = this::class.simpleName ?: "UnknownToken"
}
