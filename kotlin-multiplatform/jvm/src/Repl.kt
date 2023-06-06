package jvm

import Lexer

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
