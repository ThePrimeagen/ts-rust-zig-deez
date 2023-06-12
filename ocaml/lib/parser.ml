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
  | `Index
  ]
[@@deriving show, ord]

let prec_gte a b = compare_precedence a b >= 0

let token_prec : Token.t -> precedence = function
  | Equal | NotEqual -> `Equals
  | LessThan | GreaterThan -> `LessGreater
  | Plus | Minus -> `Sum
  | Slash | Asterisk -> `Product
  | LeftParen -> `Call
  | LeftBracket -> `Index
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

let expect_colon parser =
  expect_peek parser (function
    | Token.Colon -> true
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

let expect_rbracket parser =
  expect_peek parser (function
    | Token.RightBracket -> true
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
    | Some _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> parse' (advance parser) (stmt :: statements)
       | Error msg -> err parser msg statements)
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.current with
  | Some Token.Let -> parse_let parser
  | Some Token.Return -> parse_return parser
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more tokens"

and parse_let parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  (* move parser onto the beginning of the expression *)
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { name; value })

and parse_return parser =
  (* Move parser onto expression *)
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
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
    | Some Token.RightBrace -> Ok (parser, List.rev statements)
    | Some _ ->
      let* parser, statement = parse_statement parser in
      parse_block' (advance parser) (statement :: statements)
    | None -> Error "unexpected eof"
  in
  let* parser, block = parse_block' parser [] in
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
  | Token.String _ -> expr_parse_string parser |> map_parser
  | Token.Bang -> expr_parse_prefix parser token
  | Token.Minus -> expr_parse_prefix parser token
  | Token.True -> expr_parse_bool parser token
  | Token.False -> expr_parse_bool parser token
  | Token.LeftParen -> expr_parse_grouped parser
  | Token.If -> expr_parse_if parser
  | Token.Function -> expr_parse_fn parser
  | Token.Macro -> expr_parse_macro parser
  | Token.LeftBracket -> expr_parse_array_literal parser
  | Token.LeftBrace -> expr_parse_hash_literal parser
  | tok -> Error (Fmt.str "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and parse_call_expression parser fn =
  parse_list_of_exprs parser ~close:Token.RightParen ~final:(fun args ->
    Ast.Call { fn; args })

and parse_index_expression parser left =
  let parser = advance parser in
  let* parser, index = parse_expression parser `Lowest in
  let* parser = expect_rbracket parser in
  Ok (parser, Ast.Index { left; index })

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
  | Some LeftParen -> Some parse_call_expression
  | Some LeftBracket -> Some parse_index_expression
  | _ -> None

and expr_parse_identifier parser =
  match parser.current with
  | Some (Ident identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing number"

and expr_parse_string parser =
  match parser.current with
  | Some (String str) -> Ok (Ast.String str)
  | _ -> Error "missing string"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) ->
    let num =
      try Int.of_string num with
      | Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
    in
    Ok (Ast.Integer num)
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

and parse_list_of_exprs parser ~close ~final =
  let rec parse' parser exprs =
    match parser.peek with
    | Some tok when phys_equal close tok -> Ok (advance parser, final (List.rev exprs))
    | Some Token.Comma ->
      let parser = advance parser in
      let parser = advance parser in
      let* parser, expr = parse_expression parser `Lowest in
      parse' parser (expr :: exprs)
    | _ -> Error "unexpected next token"
  in
  match parser.peek with
  | Some tok when phys_equal close tok -> parse' parser []
  | Some _ ->
    let parser = advance parser in
    let* parser, expr = parse_expression parser `Lowest in
    parse' parser [ expr ]
  | None -> Error "hit eof"

and expr_parse_array_literal parser =
  parse_list_of_exprs parser ~close:Token.RightBracket ~final:(fun exprs ->
    Ast.Array exprs)

and expr_parse_hash_literal parser =
  let rec parse' parser exprs =
    let empty = List.length exprs = 0 in
    match parser.peek with
    | Some Token.RightBrace -> Ok (advance parser, Ast.Hash (List.rev exprs))
    | _ when empty -> parse_key_value parser exprs
    | Some Token.Comma when not empty -> parse_key_value (advance parser) exprs
    | _ -> Error "unexpected next token"
  and parse_key_value parser exprs =
    let parser = advance parser in
    let* parser, key = parse_expression parser `Lowest in
    let* parser = expect_colon parser in
    let parser = advance parser in
    let* parser, value = parse_expression parser `Lowest in
    parse' parser ((key, value) :: exprs)
  in
  parse' parser []

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

and read_identifier parser =
  match parser.current with
  | Some (Token.Ident identifier) -> Ok Ast.{ identifier }
  | _ -> Error "expected to read identifier"

and expr_parse_fn parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })

and expr_parse_macro parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.Macro { parameters; body })

and parse_list_of_parameters parser parameters =
  match parser.peek with
  | Some Token.RightParen -> Ok (advance parser, List.rev parameters)
  | Some Token.Comma ->
    let parser = advance parser in
    let parser = advance parser in
    let* ident = read_identifier parser in
    parse_list_of_parameters parser (ident :: parameters)
  | Some tok -> Error (Fmt.str "unexpected next parameter token %a" Token.pp tok)
  | None -> Error "unexpected end of stream"
;;

let string_of_statement = function
  | Ast.Let stmt ->
    Fmt.str
      "LET: let %s = %s"
      (Ast.show_identifier stmt.name)
      (show_expression stmt.value)
  | Return expr -> Fmt.str "RETURN %s" (show_expression expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)
  | BlockStatement _ -> assert false

and string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
    Fmt.pr "Program: [@.";
    List.iter program.statements ~f:(fun s -> Fmt.pr "  %s@." (string_of_statement s));
    Fmt.pr "]@."
  | _ -> failwith "yaya"
;;

module Tests = struct
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
      LET: let { identifier = "x" } = (Integer 5)
      LET: let { identifier = "y" } = (Identifier { identifier = "foo" })
      LET: let { identifier = "a" } = (Boolean true)
      LET: let { identifier = "b" } = (Boolean false)
    ] |}]
  ;;

  let%expect_test "single let statement" =
    expect_program "let x = 5;";
    [%expect {|
    Program: [
      LET: let { identifier = "x" } = (Integer 5)
    ] |}]
  ;;

  let%expect_test "expression statement" =
    expect_program "35;";
    [%expect {|
    Program: [
      EXPR: (Integer 35);
    ] |}]
  ;;

  let%expect_test "let statement with infix" =
    expect_program "let x = 1 + 2;";
    [%expect
      {|
    Program: [
      LET: let { identifier = "x" } = Infix {left = (Integer 1); operator = Token.Plus; right = (Integer 2)}
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
      [Let {name = { identifier = "z" };
         value = Prefix {operator = Token.Bang; right = (Integer 10)}};
        Let {name = { identifier = "y" }; value = (Integer 10)};
        Let {name = { identifier = "x" }; value = (Integer 5)}]
      } |}]
  ;;

  let%expect_test "grouped expressions" =
    expect_program "((1 + foo) *   12)";
    [%expect
      {|
    Program: [
      EXPR: Infix {
      left =
      Infix {left = (Integer 1); operator = Token.Plus;
        right = (Identifier { identifier = "foo" })};
      operator = Token.Asterisk; right = (Integer 12)};
    ] |}]
  ;;

  let%expect_test "if expressions" =
    expect_program "if (x < y) { x }";
    expect_program "if (x < y) { x } else { y }";
    expect_program "if (x < y) { return x } else { return y }";
    [%expect
      {|
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LessThan;
        right = (Identifier { identifier = "y" })};
      consequence =
      { block = [(ExpressionStatement (Identifier { identifier = "x" }))] };
      alternative = None};
    ]
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LessThan;
        right = (Identifier { identifier = "y" })};
      consequence =
      { block = [(ExpressionStatement (Identifier { identifier = "x" }))] };
      alternative =
      (Some { block = [(ExpressionStatement (Identifier { identifier = "y" }))] })};
    ]
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LessThan;
        right = (Identifier { identifier = "y" })};
      consequence = { block = [(Return (Identifier { identifier = "x" }))] };
      alternative =
      (Some { block = [(Return (Identifier { identifier = "y" }))] })};
    ] |}]
  ;;

  let%expect_test "function literals" =
    expect_program "fn(x, y) { return x + y; }";
    [%expect
      {|
      Program: [
        EXPR: FunctionLiteral {parameters = [{ identifier = "x" }; { identifier = "y" }];
        body =
        { block =
          [(Return
              Infix {left = (Identifier { identifier = "x" });
                operator = Token.Plus; right = (Identifier { identifier = "y" })})
            ]
          }};
      ] |}]
  ;;

  let%expect_test "function calls" =
    expect_program "let x = add(a, b);";
    expect_program "let x = empty();";
    expect_program "let x = single(a);";
    [%expect
      {|
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "add" });
      args =
      [(Identifier { identifier = "a" }); (Identifier { identifier = "b" })]}
    ]
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "empty" }); args = []}
    ]
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "single" });
      args = [(Identifier { identifier = "a" })]}
    ] |}]
  ;;

  let%expect_test "some infixes" =
    expect_program "(1 < 2) == true;";
    [%expect
      {|
      Program: [
        EXPR: Infix {
        left =
        Infix {left = (Integer 1); operator = Token.LessThan; right = (Integer 2)};
        operator = Token.Equal; right = (Boolean true)};
      ] |}]
  ;;

  let%expect_test "string parse" =
    expect_program "let x = \"hello, world!!\";";
    [%expect
      {|
      Program: [
        LET: let { identifier = "x" } = (String "hello, world!!")
      ] |}]
  ;;

  let%expect_test "array parse" =
    expect_program "[1, 2, fn (x) { x }];";
    [%expect
      {|
      Program: [
        EXPR: (Array
         [(Integer 1); (Integer 2);
           FunctionLiteral {parameters = [{ identifier = "x" }];
             body =
             { block = [(ExpressionStatement (Identifier { identifier = "x" }))] }}
           ]);
      ] |}]
  ;;

  let%expect_test "indexing" =
    expect_program "[1, 2, 3][1 + 1];";
    [%expect
      {|
      Program: [
        EXPR: Index {left = (Array [(Integer 1); (Integer 2); (Integer 3)]);
        index =
        Infix {left = (Integer 1); operator = Token.Plus; right = (Integer 1)}};
      ] |}]
  ;;

  let%expect_test "hash literals" =
    expect_program "{};";
    expect_program "{ 1: true, \"hello\": 17, true: false };";
    [%expect
      {|
      Program: [
        EXPR: (Hash []);
      ]
      Program: [
        EXPR: (Hash
         [((Integer 1), (Boolean true)); ((String "hello"), (Integer 17));
           ((Boolean true), (Boolean false))]);
      ] |}]
  ;;

  let%expect_test "precedence comparisons" =
    let cmp a b =
      Fmt.pr "%a > %a -> %b@." pp_precedence a pp_precedence b (prec_gte a b)
    in
    cmp `Lowest `Call;
    cmp `Lowest `Lowest;
    [%expect {|
    `Lowest > `Call -> false
    `Lowest > `Lowest -> true
      |}]
  ;;

  let%expect_test "macro" =
    expect_program "macro(x, y) { quote(y) }";
    [%expect
      {|
      Program: [
        EXPR: Macro {parameters = [{ identifier = "x" }; { identifier = "y" }];
        body =
        { block =
          [(ExpressionStatement
              Call {fn = (Identifier { identifier = "quote" });
                args = [(Identifier { identifier = "y" })]})
            ]
          }};
      ] |}]
  ;;
end
