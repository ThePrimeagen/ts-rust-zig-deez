open Base

let monkey_true = MonkeyObject.monkey_true
let monkey_false = MonkeyObject.monkey_false

(* Chapter 3.8 is so easy with results! It happened accidentally :) *)

let ( let* ) res f = Base.Result.bind res ~f
let map_error res = Result.map_error ~f:(fun _ -> "it failed LUL") res

let opt_to_result msg opt =
  match opt with
  | Some res -> Ok res
  | None -> Error msg
;;

let obj_is_truthy (obj : MonkeyObject.t) =
  match obj with
  | MonkeyObject.Boolean res -> res
  | Null -> false
  | _ -> true
;;

let obj_equal a b =
  (* let open MonkeyObject in *)
  (* match a, b with *)
  (* | Boolean a, Boolean b -> a = b *)
  (* | Integer a, Integer b -> a = b *)
  (* | String a, String b -> a = b *)
  (* | Array a, Array b -> a = b *)
  (* | _, _ -> a = b *)
  let ( = ) = Stdlib.( = ) in
  a = b
;;

let some_or_null opt =
  match opt with
  | Some x -> x
  | None -> MonkeyObject.Null
;;

let builtins =
  let open MonkeyObject in
  let env = Environment.init () in
  Environment.set env "len"
  @@ builtin_fn (function
       | [ String str ] -> Ok (Integer (String.length str))
       | [ Array elts ] -> Ok (Integer (List.length elts))
       | args when List.length args > 1 -> Error "must only pass one arg to len"
       | _ -> Fmt.error "bad type passed to len");
  Environment.set env "first"
  @@ builtin_fn (function
       | [ Array elts ] -> Ok (List.hd elts |> some_or_null)
       | _ -> Error "first: must be an array");
  Environment.set env "last"
  @@ builtin_fn (function
       | [ Array elts ] -> Ok (List.last elts |> some_or_null)
       | _ -> Error "last: must be an array");
  Environment.set env "rest"
  @@ builtin_fn (function
       | [ Array (_ :: rest) ] -> Ok (Array rest)
       | [ Array _ ] -> Ok Null
       | _ -> Error "rest: must be an array");
  Environment.set env "push"
  @@ builtin_fn (function
       | [ Array elts; obj ] -> Ok (Array (elts @ [ obj ]))
       | hd :: _ -> Fmt.error "push: bad head value: %a" MonkeyObject.pp hd
       | _ -> Error "push: must be an array");
  Environment.set env "puts"
  @@ builtin_fn (fun args ->
       List.iter args ~f:(fun arg -> Fmt.pr "%a@." MonkeyObject.pp arg);
       Ok Null);
  env
;;

let rec eval_input input =
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let* program = Parser.parse parser |> map_error in
  eval program builtins

and eval node env =
  match node with
  | Ast.Program program -> eval_program program.statements env
  | Ast.Expression expr -> eval_expr expr env
  | Ast.Statement stmt -> eval_statement stmt env
  | _ -> Error "oh no"

and eval_statement stmt env =
  match stmt with
  | Ast.ExpressionStatement expr -> eval_expr expr env
  | Ast.BlockStatement { block } -> eval_block block env
  | Ast.Return expr ->
    let* result = eval_expr expr env in
    Ok (MonkeyObject.Return result)
  | Ast.Let { name; value } ->
    let* value = eval_expr value env in
    Environment.set env name.identifier value;
    Ok MonkeyObject.Null
  | _ -> Error "Did not handle this statement:"

and eval_program statements env =
  List.fold_until
    statements
    ~init:(Ok MonkeyObject.Null)
    ~f:(fun _ stmt ->
      match eval_statement stmt env with
      (* Unpack returns to regular objects *)
      | Ok (Return obj) -> Stop (Ok obj)
      (* Stop executing after we get an error *)
      | Error _ as err -> Stop err
      (* Otherwise keep going! *)
      | res -> Continue res)
    ~finish:(fun stmt -> stmt)

and eval_block block env =
  List.fold_until
    block
    ~init:(Ok MonkeyObject.Null)
    ~f:(fun _ stmt ->
      match eval (Ast.Statement stmt) env with
      (* Don't unpack Returns, but must stop iterating *)
      | Ok (Return _ as ret) -> Stop (Ok ret)
      (* Stop executing after we get an error *)
      | Error _ as err -> Stop err
      (* Otherwise keep going :) *)
      | res -> Continue res)
    ~finish:(fun stmt -> stmt)

and eval_expr expr env =
  match expr with
  | Ast.Boolean true -> Ok monkey_true
  | Ast.Boolean false -> Ok monkey_false
  | Ast.Integer int -> Ok (Integer int)
  | Ast.String str -> Ok (String str)
  | Ast.Identifier identifier -> eval_identifier identifier env
  | Ast.Prefix { operator = Token.Bang; right } ->
    let* right = eval_expr right env in
    eval_bang right
  | Ast.Prefix { operator = Token.Minus; right } ->
    let* right = eval_expr right env in
    eval_minus right
  | Ast.Infix { left; operator; right } ->
    let* left = eval_expr left env in
    let* right = eval_expr right env in
    eval_infix operator left right
  | Ast.If { condition; consequence; alternative } ->
    let* condition = eval_expr condition env in
    (match condition, alternative with
     | condition, _ when obj_is_truthy condition -> eval_block consequence.block env
     | _, Some alternative -> eval_block alternative.block env
     | _, _ -> Ok Null)
  | Ast.FunctionLiteral { parameters; body } ->
    Ok (MonkeyObject.Function { parameters; body; env = Environment.enclosed env })
  | Ast.Call { fn; args } ->
    let* fn = eval_expr fn env in
    let* args =
      List.fold_until
        args
        ~init:[]
        ~f:(fun accum arg ->
          match eval_expr arg env with
          | Ok arg -> Continue (arg :: accum)
          | _ -> Stop (Error "failed to eval somethin"))
        ~finish:(fun elts -> Ok (List.rev elts))
    in
    apply_function fn args
  | Ast.Array elements ->
    let* elements =
      List.fold_until
        elements
        ~init:[]
        ~f:(fun acc elt ->
          match eval_expr elt env with
          | Ok expr -> Continue (expr :: acc)
          | _ -> Stop (Error "oh no"))
        ~finish:(fun elts -> Ok (List.rev elts))
    in
    Ok (MonkeyObject.Array elements)
  | Ast.Hash hash ->
    let* elements =
      List.fold_until
        hash
        ~init:[]
        ~f:(fun acc (key, value) ->
          let key = eval_expr key env in
          let value = eval_expr value env in
          match key, value with
          | Ok key, Ok value -> Continue ((key, value) :: acc)
          | _ -> Stop (Error "failed to eval"))
        ~finish:(fun elts -> Ok (List.rev elts))
    in
    Ok (MonkeyObject.Hash elements)
  | Ast.Index { left; index } ->
    let* left = eval_expr left env in
    let* right = eval_expr index env in
    begin
      match left, right with
      | MonkeyObject.Array lst, MonkeyObject.Integer int ->
        (match List.nth lst int with
         | Some obj -> Ok obj
         | None -> Ok Null)
      | Array _, _ -> Error "not a valid array index"
      | Hash hash, obj ->
        (match List.find hash ~f:(fun (key, _) -> obj_equal obj key) with
         | Some (_, value) -> Ok value
         | None -> Ok Null)
      | left, _ -> Fmt.error "not a valid list: %a" MonkeyObject.pp left
    end
  | expr -> Fmt.error "unhandled expr: %s" (Ast.show_expression expr)

and apply_function fn args =
  match fn with
  | MonkeyObject.Function fn ->
    let env = extend_env fn args in
    let* evaluated = eval_block fn.body.block env in
    Ok (unwrap_return evaluated)
  | Builtin (BuiltinFn fn) -> fn args
  | obj -> Fmt.error "bad function: %a" MonkeyObject.pp obj

and extend_env fn args =
  List.foldi fn.parameters ~init:(Environment.enclosed fn.env) ~f:(fun idx env arg ->
    Environment.set env arg.identifier (List.nth_exn args idx);
    env)

and unwrap_return expr =
  match expr with
  | MonkeyObject.Return expr -> expr
  | expr -> expr

and eval_identifier identifier env =
  match Environment.get env identifier.identifier with
  | Some value -> Ok value
  | None -> Error "missing identifier"

and eval_infix operator left right =
  match operator, left, right with
  | _, Integer left, Integer right -> eval_integer_infix operator left right
  | _, String left, String right -> eval_str_infix operator left right
  | Token.Equal, left, right -> Ok (Boolean (phys_equal left right))
  | Token.NotEqual, left, right -> Ok (Boolean (not @@ phys_equal left right))
  | _, left, right ->
    Fmt.error
      "unhandled infix: %s %s %s"
      (MonkeyObject.show left)
      (Token.show operator)
      (MonkeyObject.show right)

and eval_str_infix operator left right =
  match operator with
  | Token.Plus -> Ok (MonkeyObject.String (left ^ right))
  | _ -> Error "unexpected str infix"

and eval_integer_infix operator left right =
  let make_int op a b = MonkeyObject.Integer (op a b) in
  let make_bool op a b = MonkeyObject.Boolean (op a b) in
  let* maker =
    match operator with
    | Token.Plus -> Ok (make_int ( + ))
    | Token.Minus -> Ok (make_int ( - ))
    | Token.Asterisk -> Ok (make_int ( * ))
    | Token.Slash -> Ok (make_int ( / ))
    | Token.LessThan -> Ok (make_bool ( < ))
    | Token.GreaterThan -> Ok (make_bool ( > ))
    | Token.Equal -> Ok (make_bool Caml.( == ))
    | Token.NotEqual -> Ok (make_bool Caml.( != ))
    | tok -> Fmt.error "unexpected int infix op: %a" Token.pp tok
  in
  Ok (maker left right)

and eval_bang = function
  | x when phys_equal x monkey_true -> Ok monkey_false
  | x when phys_equal x monkey_false -> Ok monkey_true
  | Null -> Ok monkey_true
  | _ -> Ok monkey_false

and eval_minus = function
  | Integer int -> Ok (Integer (-int))
  | _ -> Error "nah, you can't do that man"
;;

module Test = struct
  let expect_int input =
    match eval_input input with
    | Ok (Integer i) -> Fmt.pr "%d\n" i
    | Ok _ -> Fmt.pr "MISSING THE TYPE"
    | Error msg -> Fmt.failwith "%s" msg
  ;;

  let expect_bool input =
    match eval_input input with
    | Ok (Boolean b) -> Fmt.pr "%b\n" b
    | Ok expr -> Fmt.pr "WRONG TYPES: %s@." (MonkeyObject.show expr)
    | Error msg -> Fmt.failwith "%s" msg
  ;;

  let expect_str input =
    match eval_input input with
    | Ok (String str) -> Fmt.pr "%s\n" str
    | Ok expr -> Fmt.pr "WRONG TYPES: %s@." (MonkeyObject.show expr)
    | Error msg -> Fmt.failwith "%s" msg
  ;;

  let expect_err input =
    match eval_input input with
    | Ok expr -> Fmt.failwith "should have failed: %s@." (MonkeyObject.show expr)
    | Error msg -> Fmt.pr "%s" msg
  ;;

  let%expect_test "eval integer" =
    expect_int "5;";
    expect_int "10;";
    expect_int "-10;";
    expect_int "-(-10);";
    [%expect {|
      5
      10
      -10
      10 |}]
  ;;

  let%expect_test "eval bool" =
    let _ =
      expect_bool "true;";
      [%expect {| true |}]
    in
    let _ =
      expect_bool "false;";
      [%expect {| false |}]
    in
    let _ =
      expect_bool "!false;";
      [%expect {| true |}]
    in
    let _ =
      expect_bool "!!!false;";
      [%expect {| true |}]
    in
    ()
  ;;

  let%expect_test "infix expressions" =
    expect_int "2 * 5 + 5 + (3 * 5);";
    expect_int "(5 + 10 * 2 + 15 / 3) * 2 + -10;";
    expect_bool "(1 < 2) == true;";
    expect_bool "(1 > 2) != true;";
    expect_bool "(10 + 2) * 30 == 300 + 20 * 3;";
    [%expect {|
      30
      50
      false
      true
      true |}]
  ;;

  let%expect_test "if expressions" =
    expect_int "if (5 * 5 + 10 > 34) { 99 } else { 100 };";
    expect_int "if ((1000 / 2) + 250 * 2 == 1000) { 9999 };";
    [%expect {|
      99
      9999 |}]
  ;;

  let%expect_test "return statement" =
    expect_int
      {|
      if (10 > 1) {
        if (10 > 1) {
          return 10;
        }
        return 1;
      }
    |};
    [%expect {| 10 |}]
  ;;

  let%expect_test "identifiers" =
    expect_int "let a = 5; a;";
    expect_int "let a = 5; let b = a; let c = a + b + 5; c;";
    [%expect {|
      5
      15 |}]
  ;;

  let%expect_test "function literals" =
    expect_int "let identity = fn(x) { x; }; identity(5);";
    expect_int "let multiply = fn(x, y) { x * y }; multiply(50 / 2, 1 * 2);";
    expect_bool "fn(x) { x == 10 }(10);";
    [%expect {|
      5
      50
      true |}]
  ;;

  let%expect_test "string eval" =
    expect_str "let identity = fn(x) { x; }; identity(\"hello\");";
    expect_str "let a = \"hello\"; let b = \" world\"; a + b;";
    [%expect {|
      hello
      hello world |}]
  ;;

  let%expect_test "builtin fns" =
    expect_int "len(\"hello world\");";
    [%expect {| 11 |}]
  ;;

  let%expect_test "fail with bad args" =
    expect_err "len(\"hello\", \"world\");";
    [%expect {| must only pass one arg to len |}]
  ;;

  let%expect_test "arrays" =
    expect_int "len([1, 2 * 2, 3 + 3]);";
    expect_int "[1, 2 * 2, 3 + 3][1 + 1];";
    expect_int {|
      let a = [1, 2, 3, 4];
      let b = push(a, 5);
      b[4];
    |};
    expect_int
      {|
      let a = [1, 2, 3, 4];
      let b = push(a, 5);
      len(a);
    |};
    expect_int
      {|
      let map = fn(arr, f) {
        let iter = fn(arr, accumulated) {
          if (len(arr) == 0) {
            accumulated
          } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
          }
        };
        iter(arr, []);
      };
      let a = [1, 2, 3];
      let double = fn(x) { x * 2 };
      map(a, double)[1];
    |};
    [%expect {|
      3
      6
      5
      4
      4 |}]
  ;;

  let%expect_test "function call" =
    expect_int
      {|
      let add = fn(a, b) { a + b };
      let applyFunc = fn(a, b, func) { func(a, b) };
      applyFunc(2, 2, add);
    |};
    [%expect {| 4 |}]
  ;;

  let%expect_test "hashes" =
    expect_int {| let x = { "hello": true, 5: 5 }; x[5]; |};
    expect_bool {| let x = { true: false, 5: 5 }; x[true]; |};
    expect_bool {| let x = { "hello": true, 5: 5 }; x["hello"]; |};
    [%expect {|
      5
      false
      true |}]
  ;;

  let%expect_test "puts" =
    eval_input
      {|
      puts(5);
      puts("hello world");
      puts([1, 2, 3]);
      puts(fn(x) { x + x });
    |}
    |> ignore;
    [%expect
      {|
      (Integer 5)
      (String "hello world")
      (Array [(Integer 1); (Integer 2); (Integer 3)])
      (Function
         { parameters = [{ identifier = "x" }];
           body =
           { block =
             [(ExpressionStatement
                 Infix {left = (Identifier { identifier = "x" });
                   operator = Token.Plus; right = (Identifier { identifier = "x" })})
               ]
             };
           env = <opaque> }) |}]
  ;;
end
