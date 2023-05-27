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

    fun readUntil(skipCurrent: Boolean = false, predicate: (Char) -> Boolean): String {
        skipWhitespace()
        if (skipCurrent) position++
        val oldPosition = position
        while (position < value.length && !predicate(value[position])) position++
        return value.substring(oldPosition, position)
    }

    fun readTo(skipCurrent: Boolean = false, predicate: (Char) -> Boolean): String {
        val result = readUntil(skipCurrent, predicate)
        position++
        return result
    }

    private fun skipWhitespace() {
        while(value[position].isWhitespace()) position++
    }
}