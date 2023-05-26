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

let prec_gte a b = compare_precedence a b >= 0

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
    else Error (Fmt.failwith "missing peeked: %a" pp parser)
  | None -> Error "no peek token"
;;

let peek_is parser token = Option.equal Token.equal parser.peek (Some token)

let expect_assign parser =
  expect_peek parser (function
    | Token.Assign -> true
    | _ -> false)
;;

let expect_lparen parser =
  expect_peek parser (function
    | Token.LeftParen -> true
    | _ -> false)
;;

let expect_rparen parser =
  expect_peek parser (function
    | Token.RightParen -> true
    | _ -> false)
;;

let expect_lbrace parser =
  expect_peek parser (function
    | Token.LeftBrace -> true
    | _ -> false)
;;

let expect_rbrace parser =
  expect_peek parser (function
    | Token.RightBrace -> true
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
  | Some Token.Let -> parse_let parser
  | Some Token.Return -> parse_return parser
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more toks"

and parse_let parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  (* move parser onto the beginning of the expression *)
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { name; value })

and parse_return parser =
  let* parser, expr = parse_expression parser `Lowest in
  Ok (parser, Ast.Return expr)

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_identifier parser =
  match parser.peek with
  | Some (Ident identifier) -> Ok (advance parser, { identifier })
  | _ -> Error "missing ident"

and parse_block parser =
  let parser = advance parser in
  let rec parse_block' parser statements =
    match parser.current with
    | Some Token.RightBrace | None -> Ok (parser, statements)
    | _ ->
      let* parser, statement = parse_statement parser in
      parse_block' (advance parser) (statement :: statements)
  in
  let* parser, block = parse_block' parser [] in
  let block = List.rev block in
  Ok (parser, Ast.{ block })

and parse_expression parser prec =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked = parser.peek |> Option.value ~default:Token.Illegal in
    let prec_peek = token_prec peeked in
    if peek_is parser Token.Semicolon || prec_gte prec prec_peek
    then Ok (parser, left)
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
  let map_parser = Result.map ~f:(fun v -> parser, v) in
  let token = parser.current |> Option.value_exn in
  match token with
  | Token.Ident _ -> expr_parse_identifier parser |> map_parser
  | Token.Integer _ -> expr_parse_number parser |> map_parser
  | Token.Bang -> expr_parse_prefix parser token
  | Token.Minus -> expr_parse_prefix parser token
  | Token.True -> expr_parse_bool parser token
  | Token.False -> expr_parse_bool parser token
  | Token.LeftParen -> expr_parse_grouped parser
  | Token.If -> expr_parse_if parser
  | Token.Function -> expr_parse_fn parser
  | tok -> Error (Fmt.failwith "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and get_infix_fn parser =
  let open Token in
  match parser.peek with
  | Some Plus
  | Some Minus
  | Some Slash
  | Some Asterisk
  | Some Equal
  | Some NotEqual
  | Some LessThan
  | Some GreaterThan -> Some parse_infix_expression
  | _ -> None

and expr_parse_identifier parser =
  match parser.current with
  | Some (Ident identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing number"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) -> Ok (Ast.Integer (Int.of_string num))
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

and expr_parse_if parser =
  let* parser = expect_lparen parser in
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_rparen parser in
  let* parser = expect_lbrace parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek with
    | Some Token.Else ->
      let parser = advance parser in
      let* parser = expect_lbrace parser in
      let* parser, block = parse_block parser in
      Ok (parser, Some block)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and expr_parse_fn parser =
  let _read_identifier parser =
    match parser.current with
    | Some (Token.Ident identifier) -> Ok Ast.{ identifier }
    | _ -> Error "expected to read identifier"
  in
  let expr_parse_fn_parameters parser =
    match parser.peek with
    | Some Token.RightParen -> Ok (advance parser, [])
    | Some (Token.Ident identifier) ->
      let identifier = Ast.{ identifier } in
      let parameters = ref [ identifier ] in
      (* while peek_is parser Token.Comma do *)
      (*   let parser = advance parser in *)
      (*   let parser = advance parser in *)
      (*   let* parser, identifer = read_identifier parser in *)
      (*   parameters := parameters @ [ identifier ] *)
      (* done; *)
      Ok (parser, !parameters)
    | _ -> Error "musst be identifier for parameter list"
  in
  let* parser = expect_lparen parser in
  let* parser, parameters = expr_parse_fn_parameters parser in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })
;;

let rec string_of_statement = function
  | Ast.Let stmt ->
    Fmt.str "LET: let %s = %s" (string_of_ident stmt.name) (show_expression stmt.value)
  | Return expr -> Fmt.str "RETURN %s" (show_expression expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)
  | BlockStatement _ -> assert false

and string_of_ident ident = ident.identifier

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
    Program: [
      LET: let x = (Ast.Integer 5)
      LET: let y = (Ast.Identifier { Ast.identifier = "foo" })
      LET: let a = (Ast.Boolean true)
      LET: let b = (Ast.Boolean false)
    ] |}]
;;

let%expect_test "single let statement" =
  expect_program "let x = 5;";
  [%expect {|
    Program: [
      LET: let x = (Ast.Integer 5)
    ] |}]
;;

let%expect_test "expression statement" =
  expect_program "35;";
  [%expect {|
    Program: [
      EXPR: (Ast.Integer 35);
    ] |}]
;;

let%expect_test "let statement with infix" =
  expect_program "let x = 1 + 2;";
  [%expect
    {|
    Program: [
      LET: let x = Ast.Infix {left = (Ast.Integer 1); operator = Token.Plus;
      right = (Ast.Integer 2)}
    ] |}]
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
    { Parser.msg = "missing ident";
      parser =
      { Parser.lexer =
        { Lexer.input = "\nlet x = 5;\nlet y = 10;\nlet z = !10;\nlet 838383; ";
          position = 47; ch = (Some ';') };
        current = (Some Token.Let); peek = (Some (Token.Integer "838383")) };
      statements =
      [Ast.Let {name = { Ast.identifier = "z" };
         value = Ast.Prefix {operator = Token.Bang; right = (Ast.Integer 10)}};
        Ast.Let {name = { Ast.identifier = "y" }; value = (Ast.Integer 10)};
        Ast.Let {name = { Ast.identifier = "x" }; value = (Ast.Integer 5)}]
      } |}]
;;

let%expect_test "grouped expressions" =
  expect_program "((1 + foo) *   12)";
  [%expect
    {|
    Program: [
      EXPR: Ast.Infix {
      left =
      Ast.Infix {left = (Ast.Integer 1); operator = Token.Plus;
        right = (Ast.Identifier { Ast.identifier = "foo" })};
      operator = Token.Asterisk; right = (Ast.Integer 12)};
    ] |}]
;;

let%expect_test "if expressions" =
  expect_program "if (x < y) { x }";
  expect_program "if (x < y) { x } else { y }";
  [%expect
    {|
    Program: [
      EXPR: Ast.If {
      condition =
      Ast.Infix {left = (Ast.Identifier { Ast.identifier = "x" });
        operator = Token.LessThan;
        right = (Ast.Identifier { Ast.identifier = "y" })};
      consequence =
      { Ast.block =
        [(Ast.ExpressionStatement (Ast.Identifier { Ast.identifier = "x" }))] };
      alternative = None};
    ]
    Program: [
      EXPR: Ast.If {
      condition =
      Ast.Infix {left = (Ast.Identifier { Ast.identifier = "x" });
        operator = Token.LessThan;
        right = (Ast.Identifier { Ast.identifier = "y" })};
      consequence =
      { Ast.block =
        [(Ast.ExpressionStatement (Ast.Identifier { Ast.identifier = "x" }))] };
      alternative =
      (Some { Ast.block =
              [(Ast.ExpressionStatement (Ast.Identifier { Ast.identifier = "y" }))
                ]
              })};
    ] |}]
;;

let%expect_test "function literals" =
  (* expect_program "fn(x, y) { return x + y }"; *)
  [%expect {||}]
;;

let%expect_test "precedence comparisons" =
  let cmp a b = Fmt.pr "%a > %a -> %b@." pp_precedence a pp_precedence b (prec_gte a b) in
  cmp `Lowest `Call;
  cmp `Lowest `Lowest;
  [%expect {|
    `Lowest > `Call -> false
    `Lowest > `Lowest -> true |}]
;;
