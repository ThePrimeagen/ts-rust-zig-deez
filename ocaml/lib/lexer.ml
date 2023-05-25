open Base

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let advance t =
  if t.position >= String.length t.input - 1
  then { t with ch = None }
  else
    { t with ch = Some (String.get t.input (t.position + 1)); position = t.position + 1 }
;;

let peek_char t =
  if t.position >= String.length t.input - 1
  then None
  else Some (String.get t.input (t.position + 1))
;;

let seek t f =
  let rec loop t = if f t.ch then loop @@ advance t else t in
  let t = loop t in
  t, t.position
;;

let read_while t f =
  let pos_start = t.position in
  let t, pos_end =
    seek t (fun ch ->
      match ch with
      | Some character -> f character
      | None -> false)
  in
  t, String.sub t.input ~pos:pos_start ~len:(pos_end - pos_start)
;;

let is_identifier ch = Char.(ch = '_' || is_alpha ch)

let read_identifier t =
  let t, ident = read_while t is_identifier in
  t, Token.lookup_ident ident
;;

let is_number ch = Char.is_digit ch

let read_number t =
  let t, int = read_while t is_number in
  t, Token.Integer int
;;

let skip_whitespace t =
  let t, _ =
    seek t (fun ch ->
      match ch with
      | Some ch -> Char.is_whitespace ch
      | None -> false)
  in
  t
;;

let if_peeked t ch ~default ~matched =
  let result =
    match peek_char t with
    | Some peeked when Char.(peeked = ch) -> matched
    | _ -> default
  in
  advance t, result
;;

let init input =
  if String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let next_token t =
  let t = skip_whitespace t in
  let open Token in
  match t.ch with
  | None -> t, None
  | Some ch ->
    let peek = if_peeked t in
    let t, token =
      match ch with
      | ';' -> advance t, Semicolon
      | '(' -> advance t, LeftParen
      | ')' -> advance t, RightParen
      | ',' -> advance t, Comma
      | '+' -> advance t, Plus
      | '-' -> advance t, Minus
      | '/' -> advance t, Slash
      | '*' -> advance t, Asterisk
      | '<' -> advance t, LessThan
      | '>' -> advance t, GreaterThan
      | '{' -> advance t, LeftBrace
      | '}' -> advance t, RightBrace
      | '!' -> peek '=' ~default:Bang ~matched:NotEqual
      | '=' -> peek '=' ~default:Assign ~matched:Equal
      | ch when is_identifier ch -> read_identifier t
      | ch when is_number ch -> read_number t
      | ch -> Fmt.failwith "unknown char: %c" ch
    in
    t, Some token
;;

module Test = struct
  let input_to_tokens input =
    let lexer = init input in
    let tokens = Vect.create 0 Token.EOF in
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
