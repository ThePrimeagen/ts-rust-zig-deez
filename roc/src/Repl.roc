interface Repl
    exposes [run]
    imports [pf.Stdout, pf.Stdin, pf.Task, Lexer, Parser, Eval]

run =
    env <- Task.loop (Eval.newEnv {})

    {} <- Stdout.write ">> " |> Task.await
    line <- Stdin.line |> Task.await

    parseResults =
        line
        |> Str.toUtf8
        |> Lexer.lex
        |> Parser.parse

    when parseResults is
        Ok parsedData ->
            (nextEnv, val) =
                parsedData
                |> Eval.evalWithEnv env
            {} <- Eval.printValue val |> Stdout.line |> Task.await

            Task.succeed (Step nextEnv)

        Err errs ->
            {} <- errs
                |> Str.joinWith "\n\t"
                |> \e -> "Parse Errors:\n\t\(e)"
                |> Stdout.line
                |> Task.await

            Task.succeed (Step env)
