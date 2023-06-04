package dev.hermannm.monkeylang.jvm

import dev.hermannm.monkeylang.Lexer

fun main() {
    while (true) {
        print(">> ")
        val line = readln()
        val lexer = Lexer(line)
        for (token in lexer) {
            println(token)
        }
    }
}
