module Monkey.Lexer

open System

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
    { Source: string
      Position: int
      ReadPosition: int
      Char: char }


let isLetter char =
    char >= 'a' && char <= 'z'
    || char >= 'A' && char <= 'Z'
    || char = '_'

let isNumber char = char >= '0' && char <= '9'

let readChar code =
    let newChar =
        if code.ReadPosition >= code.Source.Length then
            Char.MinValue
        else
            code.Source[code.ReadPosition]

    { code with
        Position = code.ReadPosition
        ReadPosition = code.ReadPosition + 1
        Char = newChar }
        
let toCode source =
    let code =
        { Source = source
          Position = 0
          ReadPosition = 0
          Char = Char.MinValue }

    code |> readChar

let rec skipWhitespace code =
    if code.Char = ' '
        || code.Char = '\t'
        || code.Char = '\n'
        || code.Char = '\r' then
        readChar code |> skipWhitespace
    else
        code

let readWhile code predicate =
    let start = code.Position

    let rec read code =
        if code.Char |> predicate then
            readChar code |> read
        else
            code

    let code = read code
    code, code.Source.Substring(start, code.Position - start)

let readIdentifier code =
    let code, literal = readWhile code isLetter

    let ident =
        match literal with
        | "fn" -> Function
        | "let" -> Let
        | _ -> Ident(literal)
    code, ident

let readNumber code =
    let code, literal = readWhile code isNumber
    code, Int(Int32.Parse(literal))

let nextToken code =
    let code = code |> skipWhitespace

    let advance result = code |> readChar, result

    match code.Char with
    | Char.MinValue -> (code, Eof)
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


let tokenizeCode code =
    let rec loop code =
        seq {
            match code |> nextToken with
            | _, Eof -> yield Eof
            | code, token -> 
                yield token
                yield! loop code
        }
    loop code



let tokenize source = source |> toCode |> tokenizeCode
