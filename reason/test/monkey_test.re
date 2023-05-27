open Alcotest;
open Monkey;

let testable_token = testable(Token.pp, (a, b) => a == b);
let test_token_sequence = (input, expected_tokens) => {
  let rec loop = (lexer, expected_tokens, index) =>
    switch (expected_tokens) {
    | [] => ()
    | [expected_token, ...rest] =>
      let (new_lexer, token) = Lexer.next_token(lexer);

      check(testable_token, string_of_int(index), expected_token, token);

      loop(new_lexer, rest, index + 1);
    };

  loop(Lexer.init(input), expected_tokens, 0);
};

let next_token = () => {
  let input = "=+(){},;";
  let expected_tokens = [
    Token.Assign,
    Plus,
    Left_paren,
    Right_paren,
    Left_squirly,
    Right_squirly,
    Comma,
    Semicolon,
  ];

  test_token_sequence(input, expected_tokens);
};

let test_identifier = () => {
  let input = "foobar";
  let lexer = Lexer.init(input);
  let expected_token = Token.Ident("foobar");
  let (_new_lexer, token) = Lexer.next_token(lexer);

  check(testable_token, "Identifier", expected_token, token);
};

let test_integer = () => {
  let input = "123";
  let lexer = Lexer.init(input);
  let expected_token = Token.Integer("123");
  let (_new_lexer, token) = Lexer.next_token(lexer);

  check(testable_token, "Integer", expected_token, token);
};

let test_whitespace = () => {
  let input = " = ; ";
  let expected_tokens = [Token.Assign, Semicolon, Eof];

  test_token_sequence(input, expected_tokens);
};

let test_illegal_characters = () => {
  let input = "@";
  let lexer = Lexer.init(input);
  let (_new_lexer, token) = Lexer.next_token(lexer);

  check(testable_token, "Illegal character", Token.Illegal, token);
};

let test_mixed_input = () => {
  let input = {|
    let foo = 123;
    let bar = 456;
    let add = fn(x, y) {
      x + y;
    };
    add(foo, bar);
  |};
  let expected_tokens = [
    Token.Let,
    Ident("foo"),
    Assign,
    Integer("123"),
    Semicolon,
    Let,
    Ident("bar"),
    Assign,
    Integer("456"),
    Semicolon,
    Let,
    Ident("add"),
    Assign,
    Function,
    Left_paren,
    Ident("x"),
    Comma,
    Ident("y"),
    Right_paren,
    Left_squirly,
    Ident("x"),
    Plus,
    Ident("y"),
    Semicolon,
    Right_squirly,
    Semicolon,
    Ident("add"),
    Left_paren,
    Ident("foo"),
    Comma,
    Ident("bar"),
    Right_paren,
    Semicolon,
    Eof,
  ];

  test_token_sequence(input, expected_tokens);
};

let suite = [
  (
    "Lexer",
    [
      test_case("Next token", `Quick, next_token),
      test_case("Identifier", `Quick, test_identifier),
      test_case("Integer", `Quick, test_integer),
      test_case("Whitespace", `Quick, test_whitespace),
      test_case("Illegal characters", `Quick, test_illegal_characters),
      test_case("Mixed Input", `Quick, test_mixed_input),
    ],
  ),
];

let () = run("Monkey", suite);
