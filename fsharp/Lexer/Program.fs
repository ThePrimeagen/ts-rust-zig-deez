open System

module Lexer =
    type Token =
        | Ident of string
        | Int of int
        | Illegal
        | Eof
        | Equal
        | Plus
        | Comma
        | Semicolon
        | LParen
        | RParen
        | LSquirly
        | RSquirly
        | Function
        | Let

    type Code =
        { source: string
          mutable position: int
          mutable readPosition: int
          mutable char: char }

    let fromString source =
        { source = source
          position = 0
          readPosition = 0
          char = Char.MinValue }

    let isLetter char =
        char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z' || char = '_'

    let isNumber char = char >= '0' && char <= '9'

    let readChar code =
        match code.readPosition >= code.source.Length with
        | true -> code.char <- Char.MinValue
        | false -> code.char <- code.source[code.readPosition]

        code.position <- code.readPosition
        code.readPosition <- code.readPosition + 1

    let skipWhitespace code =
        while code.char = ' ' || code.char = '\t' || code.char = '\n' || code.char = '\r' do
            readChar code

    let readIdentifier code =
        let start = code.position

        while code.char |> isLetter do
            readChar code

        let literal = code.source.Substring(start, code.position - start)

        match literal with
        | "fn" -> Function
        | "let" -> Let
        | _ -> Ident(literal)

    let readNumber code =
        let start = code.position

        while code.char |> isNumber do
            readChar code

        let literal = code.source.Substring(start, code.position - start)

        Int(Int32.Parse(literal))

    let nextToken code =
        code |> skipWhitespace

        let advance result =
            code |> readChar
            result

        match code.char with
        | Char.MinValue -> Eof
        | '=' -> advance Equal
        | '+' -> advance Plus
        | ',' -> advance Comma
        | ';' -> advance Semicolon
        | '(' -> advance LParen
        | ')' -> advance RParen
        | '{' -> advance LSquirly
        | '}' -> advance RSquirly
        | char when char |> isLetter -> code |> readIdentifier
        | char when char |> isNumber -> code |> readNumber
        | _ -> advance Illegal


    let tokenize code =
        code |> readChar

        let rec loop tokens =
            match code |> nextToken with
            | Eof -> tokens @ [ Eof ]
            | token -> loop (tokens @ [ token ])

        loop []

let code =
    Lexer.fromString
        """let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);"""

code |> Lexer.tokenize |> Seq.iter (printfn "%A")
