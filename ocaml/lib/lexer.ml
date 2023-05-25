open Base

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let init input =
  if String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let rec next_token parser =
  let parser = skip_whitespace parser in
  let open Token in
  match parser.ch with
  | None -> parser, None
  | Some ch ->
    let peek = if_peeked parser in
    let parser, token =
      match ch with
      | ';' -> advance parser, Semicolon
      | '(' -> advance parser, LeftParen
      | ')' -> advance parser, RightParen
      | ',' -> advance parser, Comma
      | '+' -> advance parser, Plus
      | '-' -> advance parser, Minus
      | '/' -> advance parser, Slash
      | '*' -> advance parser, Asterisk
      | '<' -> advance parser, LessThan
      | '>' -> advance parser, GreaterThan
      | '{' -> advance parser, LeftBrace
      | '}' -> advance parser, RightBrace
      | '!' -> peek '=' ~default:Bang ~matched:NotEqual
      | '=' -> peek '=' ~default:Assign ~matched:Equal
      | ch when is_identifier ch -> read_identifier parser
      | ch when is_number ch -> read_number parser
      | ch -> Fmt.failwith "unknown char: %c" ch
    in
    parser, Some token

and advance parser =
  if parser.position >= String.length parser.input - 1
  then { parser with ch = None }
  else (
    let position = parser.position + 1 in
    { parser with position; ch = Some (String.get parser.input position) })

and peek_char parser =
  if parser.position >= String.length parser.input - 1
  then None
  else Some (String.get parser.input (parser.position + 1))

and seek parser condition =
  let rec loop parser = if condition parser.ch then loop @@ advance parser else parser in
  let parser = loop parser in
  parser, parser.position

and read_while parser condition =
  let pos_start = parser.position in
  let parser, pos_end =
    seek parser (fun ch ->
      match ch with
      | Some character -> condition character
      | None -> false)
  in
  parser, String.sub parser.input ~pos:pos_start ~len:(pos_end - pos_start)

and read_identifier parser =
  let parser, ident = read_while parser is_identifier in
  parser, Token.lookup_ident ident

and read_number parser =
  let parser, int = read_while parser is_number in
  parser, Token.Integer int

and skip_whitespace parser =
  let parser, _ =
    seek parser (fun ch ->
      match ch with
      | Some ch -> Char.is_whitespace ch
      | None -> false)
  in
  parser

and if_peeked parser ch ~default ~matched =
  let result =
    match peek_char parser with
    | Some peeked when Char.(peeked = ch) -> matched
    | _ -> default
  in
  advance parser, result

and is_identifier ch = Char.(ch = '_' || is_alpha ch)
and is_number ch = Char.is_digit ch

module Test = struct
  let input_to_tokens input =
    let lexer = init input in
    let tokens = Vect.create 0 Token.Illegal in
    let rec loop lexer =
      match next_token lexer with
      | lexer, Some token ->
        Vect.push tokens token;
        loop lexer
      | _, None -> ()
    in
    let _ = loop lexer in
    Vect.to_list tokens
  ;;

  let print_tokens tokens =
    List.iter tokens ~f:(fun token -> Fmt.pr "%s\n" @@ Token.show token)
  ;;

  let%expect_test "idents" =
    input_to_tokens "foo_bar;" |> print_tokens;
    [%expect {|
      (Token.Ident "foo_bar")
      Token.Semicolon |}]
  ;;

  let%expect_test "next token" =
    let input = "=(){},;;" in
    let tokens = input_to_tokens input in
    print_tokens tokens;
    [%expect
      {|
    Token.Assign
    Token.LeftParen
    Token.RightParen
    Token.LeftBrace
    Token.RightBrace
    Token.Comma
    Token.Semicolon
    Token.Semicolon |}]
  ;;

  let%expect_test "hello" =
    let input =
      {|
let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5; 

if (5 < 10) {
  return true;
} else {
  return false;
}
    |}
    in
    let tokens = input_to_tokens input in
    print_tokens tokens;
    [%expect
      {|
    Token.Let
    (Token.Ident "five")
    Token.Assign
    (Token.Integer "5")
    Token.Semicolon
    Token.Let
    (Token.Ident "ten")
    Token.Assign
    (Token.Integer "10")
    Token.Semicolon
    Token.Let
    (Token.Ident "add")
    Token.Assign
    Token.Function
    Token.LeftParen
    (Token.Ident "x")
    Token.Comma
    (Token.Ident "y")
    Token.RightParen
    Token.LeftBrace
    (Token.Ident "x")
    Token.Plus
    (Token.Ident "y")
    Token.Semicolon
    Token.RightBrace
    Token.Semicolon
    Token.Let
    (Token.Ident "result")
    Token.Assign
    (Token.Ident "add")
    Token.LeftParen
    (Token.Ident "five")
    Token.Comma
    (Token.Ident "ten")
    Token.RightParen
    Token.Semicolon
    Token.Bang
    Token.Minus
    Token.Slash
    Token.Asterisk
    (Token.Integer "5")
    Token.Semicolon
    (Token.Integer "5")
    Token.LessThan
    (Token.Integer "10")
    Token.GreaterThan
    (Token.Integer "5")
    Token.Semicolon
    Token.If
    Token.LeftParen
    (Token.Integer "5")
    Token.LessThan
    (Token.Integer "10")
    Token.RightParen
    Token.LeftBrace
    Token.Return
    Token.True
    Token.Semicolon
    Token.RightBrace
    Token.Else
    Token.LeftBrace
    Token.Return
    Token.False
    Token.Semicolon
    Token.RightBrace |}]
  ;;
end
