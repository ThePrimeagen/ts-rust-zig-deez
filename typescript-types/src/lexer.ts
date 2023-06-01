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
  export type Parse<Input> = Input;
}
