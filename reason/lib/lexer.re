type t = {
  input: string,
  read_position: int,
  ch: option(char),
};

let init = input => {input, read_position: 1, ch: Some(input.[0])};

let rec next_token = lexer =>
  switch (lexer.ch) {
  | Some(c) =>
    switch (c) {
    | '=' => (advance(lexer), Token.Assign)
    | ';' => (advance(lexer), Semicolon)
    | '(' => (advance(lexer), Left_paren)
    | ')' => (advance(lexer), Right_paren)
    | ',' => (advance(lexer), Comma)
    | '+' => (advance(lexer), Plus)
    | '{' => (advance(lexer), Left_squirly)
    | '}' => (advance(lexer), Right_squirly)
    | c when is_digit(c) => read_number(lexer)
    | c when is_letter(c) => read_identifier(lexer)
    | c when is_whitespace(c) => advance(lexer) |> next_token
    | _ => (advance(lexer), Illegal)
    }
  | None => (lexer, Eof)
  }

and advance = lexer =>
  if (lexer.read_position >= String.length(lexer.input)) {
    {...lexer, ch: None};
  } else {
    {
      ...lexer,
      read_position: lexer.read_position + 1,
      ch: Some(lexer.input.[lexer.read_position]),
    };
  }

and read_while = (lexer, condition) => {
  let rec loop = (lexer, acc) =>
    switch (lexer.ch) {
    | Some(c) when condition(c) => loop(advance(lexer), [c, ...acc])
    | _ => (
        acc |> List.rev |> List.map(String.make(1)) |> String.concat(""),
        lexer,
      )
    };

  loop(lexer, []);
}

and read_number = lexer => {
  let (number, lexer) = read_while(lexer, is_digit);
  (lexer, Token.Integer(number));
}

and read_identifier = lexer => {
  let (str, lexer) = read_while(lexer, is_letter);
  (lexer, Token.lookup_ident(str));
}

and is_digit =
  fun
  | '0' .. '9' => true
  | _ => false

and is_letter =
  fun
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' => true
  | _ => false

and is_whitespace =
  fun
  | ' '
  | '\t'
  | '\n'
  | '\r' => true
  | _ => false;
