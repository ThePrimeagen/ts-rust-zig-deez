module Monkey.Program

[<EntryPoint>]
let main _ =

    let code =
        """let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);"""

    code |> Lexer.tokenize |> Seq.iter (printfn "%A")

    0
