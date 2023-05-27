module Tests

open Xunit
open FsUnit.Xunit
open Program.Lexer

[<Fact>]
let ``tokenize empty`` () =
    let code = fromString ""

    let expectedTokens = [ Eof ]

    tokenize code |> should equalSeq expectedTokens

[<Fact>]
let ``tokenize simple`` () =
    let code = fromString "=+(){},;"

    let expectedTokens =
        [ Equal; Plus; LParen; RParen; LSquirly; RSquirly; Comma; Semicolon; Eof ]

    tokenize code |> should equalSeq expectedTokens

[<Fact>]
let ``tokenize simple with whitespace`` () =
    let code = fromString " = + ( ) { } , ; "

    let expectedTokens =
        [ Equal; Plus; LParen; RParen; LSquirly; RSquirly; Comma; Semicolon; Eof ]

    tokenize code |> should equalSeq expectedTokens

[<Fact>]
let ``tokenize complex`` () =
    let code =
        fromString
            """let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);"""

    let expectedTokens =
        [ Let
          Ident "five"
          Equal
          Int 5
          Semicolon
          Let
          Ident "ten"
          Equal
          Int 10
          Semicolon
          Let
          Ident "add"
          Equal
          Function
          LParen
          Ident "x"
          Comma
          Ident "y"
          RParen
          LSquirly
          Ident "x"
          Plus
          Ident "y"
          Semicolon
          RSquirly
          Semicolon
          Let
          Ident "result"
          Equal
          Ident "add"
          LParen
          Ident "five"
          Comma
          Ident "ten"
          RParen
          Semicolon
          Eof ]

    tokenize code |> should equalSeq expectedTokens
