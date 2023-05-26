open Base

let show_expression = Ast.show_expression

(** Bind operation for Base.Result *)
let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  ]
[@@deriving show, ord]

let prec_greater a b =
  (* Fmt.pr *)
  (*   "comparing: %a %a -> %d@." *)
  (*   pp_precedence *)
  (*   a *)
  (*   pp_precedence *)
  (*   b *)
  (*   (compare_precedence a b); *)
  let compared = compare_precedence a b in
  compared = 1 || compared = 0
;;

let token_prec : Token.t -> precedence = function
  | Equal | NotEqual -> `Equals
  | LessThan | GreaterThan -> `LessGreater
  | Plus | Minus -> `Sum
  | Slash | Asterisk -> `Product
  | _ -> `Lowest
;;

type t =
  { lexer : Lexer.t
  ; current : Token.t option
  ; peek : Token.t option
  }
[@@deriving show]

type parse_error =
  { msg : string
  ; parser : t
  ; statements : Ast.statement list
  }
[@@deriving show]

let err parser msg statements = Error { parser; msg; statements }

let advance parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; peek; current = parser.peek }
;;

let advance_until parser f =
  let parser = ref parser in
  while not (f !parser) do
    parser := advance !parser
  done;
  !parser
;;

let chomp_semicolon parser =
  match parser.peek with
  | Some Token.Semicolon -> advance parser
  | _ -> parser
;;

let next_token parser =
  let parser = advance parser in
  parser, parser.current
;;

let expect_peek parser condition =
  match parser.peek with
  | Some tok ->
    if condition tok
    then Ok (advance parser)
    else Error (Fmt.str "missing peeked: %a" pp parser)
  | None -> Error "no peek token"
;;

let peek_is parser token = Option.equal Token.equal parser.peek (Some token)

let expect_assign parser =
  expect_peek parser (function
    | Token.Assign -> true
    | _ -> false)
;;

let peek_precedence parser =
  match parser.peek with
  | Some tok -> token_prec tok
  | _ -> `Lowest
;;

let curr_precedence parser =
  match parser.current with
  | Some tok -> token_prec tok
  | _ -> `Lowest
;;

let init lexer =
  let parser = { lexer; current = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser
;;

let rec parse parser =
  let rec parse' parser statements =
    match parser.current with
    | Some _ -> begin
      match parse_statement parser with
      | Ok (parser, stmt) ->
        let parser = advance parser in
        parse' parser (stmt :: statements)
      | Error msg -> err parser msg statements
    end
    | None -> Ok (parser, statements)
  in
  let* _, statements = parse' parser [] in
  let statements = List.rev statements in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.current with
  | Some (Token.Let as token) -> parse_let parser token
  | Some (Token.Return as token) -> parse_return parser token
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more toks"

and parse_let parser token =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  (* move parser onto the beginning of the expression *)
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { token; name; value })

and parse_return parser token =
  let* parser, expr = parse_expression parser `Lowest in
  Ok (parser, Ast.Return { token; expr })

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_identifier parser =
  match parser.peek with
  | Some (Ident ident as token) -> Ok (advance parser, { token; value = ident })
  | _ -> Error "missing ident"

and parse_expression parser prec =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked = parser.peek |> Option.value ~default:Token.Illegal in
    Fmt.pr "parse_expr' checking: %a vs %a@." Token.pp peeked pp_precedence prec;
    let prec_peek = token_prec peeked in
    if peek_is parser Token.Semicolon || prec_greater prec prec_peek
    then (
      Fmt.pr "  parse_expr': skipped %b@." (prec_greater prec_peek prec);
      Ok (parser, left))
    else (
      match get_infix_fn parser with
      | Some infix_fn ->
        let parser = advance parser in
        let* parser, left = infix_fn parser left in
        parse_expression' parser left
      | None -> Ok (parser, left))
  in
  parse_expression' parser left

and parse_prefix_expression parser =
  let token = parser.current |> Option.value_exn in
  match token with
  | Token.Ident _ -> expr_parse_identifier parser token
  | Token.Integer _ -> expr_parse_number parser
  | Token.Bang -> expr_parse_prefix parser token
  | Token.Minus -> expr_parse_prefix parser token
  | Token.True -> expr_parse_bool parser token
  | Token.False -> expr_parse_bool parser token
  | Token.LeftParen -> expr_parse_grouped parser
  | tok -> Error (Fmt.failwith "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and get_infix_fn parser =
  let open Token in
  match parser.current with
  | Some Plus
  | Some Minus
  | Some Slash
  | Some Asterisk
  | Some Equal
  | Some NotEqual
  | Some LessThan
  | Some GreaterThan -> Some parse_infix_expression
  | _ -> None

and expr_parse_identifier parser token =
  match parser.current with
  | Some (Ident ident) -> Ok (parser, Ast.Identifier { token; value = ident })
  | _ -> Error "missing number"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) -> Ok (parser, Ast.Integer (Int.of_string num))
  | _ -> Error "missing number"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_bool parser bool =
  let* bool =
    match bool with
    | Token.True -> Ok true
    | Token.False -> Ok false
    | _ -> Error "not a valid boolean"
  in
  Ok (parser, Ast.Boolean bool)

and expr_parse_grouped parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let* parser =
    expect_peek parser (function
      | Token.RightParen -> true
      | _ -> false)
  in
  Ok (parser, expr)
;;

let rec string_of_statement = function
  | Ast.Let stmt ->
    Fmt.str "LET: let %s = %s" (string_of_ident stmt.name) (show_expression stmt.value)
  | Ast.Return stmt -> Fmt.str "RETURN %s" (show_expression stmt.expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)

and string_of_ident ident = ident.value

let print_node = function
  | Ast.Program program ->
    Fmt.pr "Program: [@.";
    List.iter program.statements ~f:(fun s -> Fmt.pr "  %s@." (string_of_statement s));
    Fmt.pr "]@."
  | _ -> failwith "yaya"
;;

let expect_program input =
  let lexer = Lexer.init input in
  let parser = init lexer in
  let program = parse parser in
  match program with
  | Ok program -> print_node program
  | Error msg -> Fmt.failwith "%a@." pp_parse_error msg
;;

let%expect_test "series of let statements" =
  expect_program {|
let x = 5;
let y = foo;
let a = true;
let b = false;
    |};
  [%expect
    {|
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    Program: [
      LET: let x = (Ast.Integer 5)
      LET: let y = (Ast.Identifier { Ast.token = (Token.Ident "foo"); value = "foo" })
      LET: let a = (Ast.Boolean true)
      LET: let b = (Ast.Boolean false)
    ] |}]
;;

let%expect_test "single let statement" =
  expect_program "let x = 5;";
  [%expect
    {|
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    Program: [
      LET: let x = (Ast.Integer 5)
    ] |}]
;;

let%expect_test "expression statement" =
  expect_program "35;";
  [%expect
    {|
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    Program: [
      EXPR: (Ast.Integer 35);
    ] |}]
;;

let%expect_test "let statement with infix" =
  expect_program "let x = 1 + 2;";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure
     "unexpected prefix expr: Token.Plus\
    \n { Parser.lexer =\
    \n                                      { Lexer.input = \"let x = 1 + 2;\";\
    \n                                        position = 13; ch = (Some ';') };\
    \n                                      current = (Some Token.Plus);\
    \n                                      peek = (Some (Token.Integer \"2\")) }")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Monkey__Parser.parse_prefix_expression in file "lib/parser.ml", line 191, characters 17-88
  Called from Monkey__Parser.parse_expression in file "lib/parser.ml", line 162, characters 22-52
  Called from Monkey__Parser.parse_expression_statement in file "lib/parser.ml", line 152, characters 22-53
  Called from Monkey__Parser.parse.parse' in file "lib/parser.ml", line 119, characters 12-34
  Called from Monkey__Parser.parse in file "lib/parser.ml", line 127, characters 23-39
  Called from Monkey__Parser.expect_program in file "lib/parser.ml", line 267, characters 16-28
  Called from Monkey__Parser.(fun) in file "lib/parser.ml", line 321, characters 2-33
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  parse_expr' checking: Token.Plus vs `Lowest |}]
;;

let%expect_test "let statement errors" =
  let input = {|
let x = 5;
let y = 10;
let z = !10;
let 838383; |} in
  let lexer = Lexer.init input in
  let parser = init lexer in
  let program = parse parser in
  begin
    match program with
    | Ok _ -> failwith "should not succeed"
    | Error msg -> Fmt.pr "%a@." pp_parse_error msg
  end;
  [%expect
    {|
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    parse_expr' checking: Token.Semicolon vs `Prefix
      parse_expr': skipped false
    parse_expr' checking: Token.Semicolon vs `Lowest
      parse_expr': skipped true
    { Parser.msg = "missing ident";
      parser =
      { Parser.lexer =
        { Lexer.input = "\nlet x = 5;\nlet y = 10;\nlet z = !10;\nlet 838383; ";
          position = 47; ch = (Some ';') };
        current = (Some Token.Let); peek = (Some (Token.Integer "838383")) };
      statements =
      [Ast.Let {token = Token.Let;
         name = { Ast.token = (Token.Ident "z"); value = "z" };
         value = Ast.Prefix {operator = Token.Bang; right = (Ast.Integer 10)}};
        Ast.Let {token = Token.Let;
          name = { Ast.token = (Token.Ident "y"); value = "y" };
          value = (Ast.Integer 10)};
        Ast.Let {token = Token.Let;
          name = { Ast.token = (Token.Ident "x"); value = "x" };
          value = (Ast.Integer 5)}
        ]
      } |}]
;;
