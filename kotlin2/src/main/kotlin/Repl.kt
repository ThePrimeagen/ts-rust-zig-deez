package dev.hermannm.monkeylang

fun main() {
    while (true) {
        print(">> ")
        val line = readln()
        val lexer = Lexer(line)

        var token = lexer.nextToken()
        while (token != Token.EndOfFile) {
            println(token)
            token = lexer.nextToken()
        }
    }
}
