package jdsl

fun main() {
    val username = System.getProperty("user.name")
    println("Hello $username! This is the Monkey programming language!")
    println("Feel free to type in commands")
    Repl().start()
}
