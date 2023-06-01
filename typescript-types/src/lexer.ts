export enum TokenType {
  Illegal,
  Eof,
  Ident = "IDENT",
  If = "if",
  Return = "return",
  True = "true",
  False = "false",
  Else = "else",
  Int = "INT",
  Assign = "=",
  NotEqual = "!=",
  Equal = "==",
  Plus = "+",
  Comma = ",",
  Semicolon = ";",
  LParen = "(",
  RParen = ")",
  LSquirly = "{",
  RSquirly = "}",
  Function = "FUNCTION",
  Let = "LET",
  Bang = "!",
  Dash = "-",
  ForwardSlash = "/",
  Asterisk = "*",
  LessThan = "<",
  GreaterThan = ">",
}

export type Token<Type extends TokenType, Literal extends string> = {
  type: Type;
  literal: Literal;
};

export namespace Tokenizer {
  type PriorityTokens = {
    "!=": TokenType.NotEqual;
    "==": TokenType.Equal;
  };
  type PriorityToken = keyof PriorityTokens;

  type TokenMap = {
    return: TokenType.Return;
    false: TokenType.False;
    else: TokenType.Else;
    true: TokenType.True;
    let: TokenType.Let;
    fn: TokenType.Function;
    if: TokenType.If;
    "!": TokenType.Bang;
    "-": TokenType.Dash;
    "/": TokenType.ForwardSlash;
    "*": TokenType.Asterisk;
    "=": TokenType.Assign;
    "<": TokenType.LessThan;
    ">": TokenType.GreaterThan;
    "+": TokenType.Plus;
    "(": TokenType.LParen;
    ")": TokenType.RParen;
    "{": TokenType.LSquirly;
    "}": TokenType.RSquirly;
    ",": TokenType.Comma;
    ";": TokenType.Semicolon;
  };
  type SomeToken = keyof TokenMap;

  type Whitespace = " " | "\n";
  type SkipWhitespace<T extends string> = T extends `${Whitespace}${infer Rest}`
    ? SkipWhitespace<Rest>
    : T;

  type IsLetter<T extends string> = T extends Capitalize<T> ? false : true;
  type ReadIdent<
    Input extends string,
    Ident extends string = ""
  > = IsLetter<Input> extends true
    ? Input extends `${infer Head}${infer Tail}`
      ? ReadIdent<Tail, `${Ident}${Head}`>
      : never
    : Ident extends ""
    ? []
    : [Ident, Input];

  type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
  type ReadInt<
    Input extends string,
    Int extends string = ""
  > = Input extends `${infer Head extends Digit}${infer Tail}`
    ? ReadInt<Tail, `${Int}${Head}`>
    : Int extends ""
    ? false
    : [Int, Input];

  type GetNextToken<Input extends string> =
    // Prio tokens
    Input extends `${PriorityToken}${infer Tail}`
      ? Input extends `${infer Head extends PriorityToken}${Tail}`
        ? [Token<PriorityTokens[Head], Head>, Tail]
        : never
      : // Other literals
      Input extends `${SomeToken}${infer Tail}`
      ? Input extends `${infer Head extends SomeToken}${Tail}`
        ? [Token<TokenMap[Head], Head>, Tail]
        : never
      : // Ident
      ReadIdent<Input> extends [infer Ident extends string, infer Tail]
      ? [Token<TokenType.Ident, Ident>, Tail]
      : // Int
      ReadInt<Input> extends [infer Int extends string, infer Tail]
      ? [Token<TokenType.Int, Int>, Tail]
      : // End
        Input;

  // TODO: this instantiates deeply with spread operator on long inputs, needs less recursion
  export type New<
    Input extends string,
    Tok = GetNextToken<SkipWhitespace<Input>>
  > = Tok extends [
    infer Next extends Token<TokenType, string>,
    infer Rest extends string
  ]
    ? [Next, New<Rest>]
    : [TokenType.Eof, "eof"];
}

export type Link<T extends any[]> = T extends [infer Item, ...infer Rest]
  ? [Item, Link<Rest>]
  : T;
