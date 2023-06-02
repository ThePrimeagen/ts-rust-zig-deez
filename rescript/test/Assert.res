open Test

let isEqual = (a, b) => a === b

let assertTokensEqual = (a: Token.t, b: Token.t) => {
  let (res, msg) = switch (a, b) {
  | (Token.Ident(a), Token.Ident(b)) => (a == b, a ++ " matches " ++ b)
  | (Token.Integer(a), Token.Integer(b)) => (a == b, a ++ "matches" ++ b)
  | _ => (a == b, "Token Match")
  }

  assertion((_, _) => res, a, b, ~message=msg)
}
