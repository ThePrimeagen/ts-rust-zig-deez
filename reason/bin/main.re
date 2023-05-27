open Monkey;

let input = {|
    let foo = 123;
    let bar = 456;
    let add = fn(x, y) {
      x + y;
    };
    add(foo, bar);
  |};

let lexer = Lexer.init(input);

let rec print_tokens = lexer => {
  let (new_lexer, token) = Lexer.next_token(lexer);

  print_endline("Token: " ++ Token.show(token));

  if (token == Token.Eof) {
    new_lexer;
  } else {
    print_tokens(new_lexer);
  };
};

print_newline();
print_tokens(lexer);
