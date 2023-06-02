import lexer
import rdstdin
import token

when isMainModule:
    const prompt = ">> "

    while true:
        let input = readLineFromStdin(prompt)
        var l = newLexer(input)
        var tok = l.nextToken()
        echo(tok)
        while tok.ty != ttEof:
            tok = l.nextToken()
            echo(tok)


