package jdsl

class Token(val type: TokenType, val literal: String? = null) {
    override fun toString(): String {
        return "$type $literal"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Token) return false

        if (type != other.type) return false
        return literal == other.literal
    }

    override fun hashCode(): Int {
        var result = type.hashCode()
        result = 31 * result + (literal?.hashCode() ?: 0)
        return result
    }
}