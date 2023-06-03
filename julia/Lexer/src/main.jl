include("lexer.jl")
using .Lexer

function main()
    input = "=+(){},;"
    lexer = LexerState(input)

    for _ = 1:10
        tok = next_token(lexer)
        println("Type: ", typeof(tok.Type))
        println("Literal: ", tok.Literal)
    end

    println("");

    input  = "let x = 5"
    lexer = LexerState(input)

    for _ = 1:10
        tok = next_token(lexer)
        println("Type: ", typeof(tok.Type))
        println("Literal: ", tok.Literal)
    end
end

main()
