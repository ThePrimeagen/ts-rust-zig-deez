type t = {
  input: string,
  position: int,
  ch: option<char>,
}

let init: string => t

let nextToken: t => (t, Token.t)
