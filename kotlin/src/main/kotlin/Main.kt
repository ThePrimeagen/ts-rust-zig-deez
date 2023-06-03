import deez.Lexer

fun main() {
    val input = "Kotl in deez"

    val lexer = Lexer(input)
    var token = lexer.nextToken()
    while (token.type != Lexer.Token.Type.EOF && token.type != Lexer.Token.Type.ILLEGAL) {
        println(token)
        token = lexer.nextToken()
    }
}