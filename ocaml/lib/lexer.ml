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

let rec next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | ';' -> advance lexer, Semicolon
      | '(' -> advance lexer, LeftParen
      | ')' -> advance lexer, RightParen
      | ',' -> advance lexer, Comma
      | '+' -> advance lexer, Plus
      | '-' -> advance lexer, Minus
      | '/' -> advance lexer, Slash
      | '*' -> advance lexer, Asterisk
      | '<' -> advance lexer, LessThan
      | '>' -> advance lexer, GreaterThan
      | '{' -> advance lexer, LeftBrace
      | '}' -> advance lexer, RightBrace
      | ':' -> advance lexer, Colon
      | '[' -> advance lexer, LeftBracket
      | ']' -> advance lexer, RightBracket
      | '!' -> if_peeked lexer '=' ~default:Bang ~matched:NotEqual
      | '=' -> if_peeked lexer '=' ~default:Assign ~matched:Equal
      | '"' -> read_string lexer
      | ch when is_identifier ch -> read_identifier lexer
      | ch when is_number ch -> read_number lexer
      | ch -> Fmt.failwith "unknown char: %c" ch
    in
    lexer, Some token

and advance lexer =
  if lexer.position >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })

and peek_char lexer =
  if lexer.position >= String.length lexer.input - 1
  then None
  else Some (String.get lexer.input (lexer.position + 1))

and seek lexer condition =
  let rec loop lexer = if condition lexer.ch then loop @@ advance lexer else lexer in
  let lexer = loop lexer in
  lexer, lexer.position

and read_while lexer condition =
  let pos_start = lexer.position in
  let lexer, pos_end =
    seek lexer (fun ch ->
      match ch with
      | Some character -> condition character
      | None -> false)
  in
  lexer, String.sub lexer.input ~pos:pos_start ~len:(pos_end - pos_start)

and read_identifier lexer =
  let lexer, ident = read_while lexer is_identifier in
  lexer, Token.lookup_ident ident

and read_number lexer =
  let lexer, int = read_while lexer is_number in
  lexer, Token.Integer int

and read_string lexer =
  let lexer = advance lexer in
  let lexer, str = read_while lexer (fun ch -> Char.(ch <> '"')) in
  let lexer = advance lexer in
  lexer, Token.String str

and skip_whitespace lexer =
  let lexer, _ =
    seek lexer (fun ch ->
      match ch with
      | Some ch -> Char.is_whitespace ch
      | None -> false)
  in
  lexer

and if_peeked lexer ch ~default ~matched =
  let lexer, result =
    match peek_char lexer with
    | Some peeked when Char.(peeked = ch) -> advance lexer, matched
    | _ -> lexer, default
  in
  advance lexer, result

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

  let%expect_test "infix lex" =
    let input = "(1 < 2) == true;" in
    let tokens = input_to_tokens input in
    print_tokens tokens;
    [%expect
      {|
      Token.LeftParen
      (Token.Integer "1")
      Token.LessThan
      (Token.Integer "2")
      Token.RightParen
      Token.Equal
      Token.True
      Token.Semicolon |}]
  ;;

  let%expect_test "strings" =
    input_to_tokens "\"hello, world!\"" |> print_tokens;
    [%expect {| (Token.String "hello, world!") |}]
  ;;

  let%expect_test "arrays" =
    input_to_tokens "[1, 2, 3];" |> print_tokens;
    [%expect
      {|
      Token.LeftBracket
      (Token.Integer "1")
      Token.Comma
      (Token.Integer "2")
      Token.Comma
      (Token.Integer "3")
      Token.RightBracket
      Token.Semicolon |}]
  ;;

  let%expect_test "hash" =
    input_to_tokens "{ 1:true };" |> print_tokens;
    [%expect
      {|
      Token.LeftBrace
      (Token.Integer "1")
      Token.Colon
      Token.True
      Token.RightBrace
      Token.Semicolon |}]
  ;;

  let%expect_test "macro" =
    input_to_tokens "macro(x, y) { x, y }" |> print_tokens;
    [%expect
      {|
      Token.Macro
      Token.LeftParen
      (Token.Ident "x")
      Token.Comma
      (Token.Ident "y")
      Token.RightParen
      Token.LeftBrace
      (Token.Ident "x")
      Token.Comma
      (Token.Ident "y")
      Token.RightBrace |}]
  ;;
end
