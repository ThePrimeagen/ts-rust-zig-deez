package jdsl

class Repl {
    private val PROMPT: String = ">> "

    fun start() {
        while (true) {
            print(PROMPT)
            val line = readlnOrNull() ?: break
            val lexer = Lexer(line)
            val tokens = lexer.readTokens()
            println(tokens)
        }
    }
}