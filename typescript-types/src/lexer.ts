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
  Semicolon = ",",
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
  type TokenMap = {
    "=": TokenType.Assign;
    "+": TokenType.Plus;
    "(": TokenType.LParen;
    ")": TokenType.RParen;
    "{": TokenType.LSquirly;
    "}": TokenType.RSquirly;
    ",": TokenType.Comma;
    ";": TokenType.Semicolon;
  };
  type GetNextToken<Input extends string> =
    Input extends `${infer Head}${infer Tail}`
      ? Head extends keyof TokenMap
        ? [Token<TokenMap[Head], Head>, Tail]
        : [TokenType.Illegal, Tail]
      : Input;

  export type New<
    Input extends string,
    Result extends any[] = [],
    NextToken = GetNextToken<Input>
  > = NextToken extends [Token<TokenType, string>, infer Tail extends string]
    ? New<Tail, [...Result, NextToken[0]]>
    : Result;
}
