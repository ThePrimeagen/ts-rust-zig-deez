import lexer

fn main() {
    input := "=+(){},;"
    lex := lexer.Lexer{input: input}
    for _, c := range input {
        println(lex.get_next_token())
    }
    println(lex.get_next_token())
}