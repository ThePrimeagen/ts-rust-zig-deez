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
      mutable Position: int
      mutable ReadPosition: int
      mutable Char: char }

let toCode source =
    { Source = source
      Position = 0
      ReadPosition = 0
      Char = Char.MinValue }

let isLetter char =
    char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z' || char = '_'

let isNumber char = char >= '0' && char <= '9'

let readChar code =
    match code.ReadPosition >= code.Source.Length with
    | true -> code.Char <- Char.MinValue
    | false -> code.Char <- code.Source[code.ReadPosition]

    code.Position <- code.ReadPosition
    code.ReadPosition <- code.ReadPosition + 1

let skipWhitespace code =
    while code.Char = ' ' || code.Char = '\t' || code.Char = '\n' || code.Char = '\r' do
        readChar code

let readWhile code predicate =
    let start = code.Position

    while code.Char |> predicate do
        readChar code

    code.Source.Substring(start, code.Position - start)

let readIdentifier code =
    let literal = readWhile code isLetter

    match literal with
    | "fn" -> Function
    | "let" -> Let
    | _ -> Ident(literal)

let readNumber code =
    let literal = readWhile code isNumber
    Int(Int32.Parse(literal))

let nextToken code =
    code |> skipWhitespace

    let advance result =
        code |> readChar
        result

    match code.Char with
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


let tokenizeCode code =
    code |> readChar

    let rec loop tokens =
        match code |> nextToken with
        | Eof -> tokens @ [ Eof ]
        | token -> loop (tokens @ [ token ])

    loop []

let tokenize source = source |> toCode |> tokenizeCode
