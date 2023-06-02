type t = {
  input: string,
  position: int,
  ch: option<char>,
}

let init = input => {
  if String.length(input) == 0 {
    {input, position: 0, ch: None}
  } else {
    {input, position: 0, ch: Some(String.get(input, 0))}
  }
}

let rec nextToken = lexer => {
  open Token
  switch lexer.ch {
  | None => (lexer, EOF)
  | Some(ch) =>
    switch ch {
    | '=' => (advance(lexer), Assign)
    | '+' => (advance(lexer), Plus)
    | '(' => (advance(lexer), LParen)
    | ')' => (advance(lexer), RParen)
    | '{' => (advance(lexer), LSquirly)
    | '}' => (advance(lexer), RSquirly)
    | ',' => (advance(lexer), Comma)
    | ';' => (advance(lexer), Semicolon)
    | ch if isDigit(ch) => readNumber(lexer)
    | ch if isAlpha(ch) => readIdent(lexer)
    | ch if isWhitespace(ch) => nextToken(advance(lexer))
    | _ => (advance(lexer), Illegal)
    }
  }
}
and advance = lexer => {
  if lexer.position >= String.length(lexer.input) - 1 {
    {...lexer, ch: None}
  } else {
    let position = lexer.position + 1
    {input: lexer.input, position, ch: Some(String.get(lexer.input, position))}
  }
}
and isDigit = ch => {
  ch >= '0' && ch <= '9'
}
and isAlpha = ch => {
  (ch >= 'a' && ch <= 'z') || ch >= 'A' && ch <= 'Z' || ch == '_'
}
and isWhitespace = ch => {
  ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}
and readWhile = (lexer, condition) => {
  let rec loop = (lexer, acc) => {
    switch lexer.ch {
    | None => (lexer, acc)
    | Some(ch) =>
      if condition(ch) {
        loop(advance(lexer), acc ++ String.make(1, ch))
      } else {
        (lexer, acc)
      }
    }
  }
  loop(lexer, "")
}
and readNumber = lexer => {
  let (lexer, number) = readWhile(lexer, isDigit)
  (lexer, Token.Integer(number))
}
and readIdent = lexer => {
  let (lexer, ident) = readWhile(lexer, isAlpha)
  (lexer, Token.lookupIdent(ident))
}
