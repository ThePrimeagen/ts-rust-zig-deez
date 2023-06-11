export enum TokenType {
  Illegal,
  Eof,
  Ident,
  Int,
  Assign,
  Plus,
  Dash,
  Bang,
  Asterisk,
  ForwardSlash,
  LessThan,
  GreaterThan,
  Equal,
  NotEqual,
  Comma,
  Semicolon,
  LParen,
  RParen,
  LSquirly,
  RSquirly,
  Function,
  Let,
  True,
  False,
  If,
  Else,
  Return,
}

export type Token<
  Type extends TokenType = TokenType,
  Literal extends string = string
> = {
  type: Type;
  literal: Literal;
};

/**
 * Main tokenizing engine.
 *
 * @template Input the input source code
 * @template Tokens gets automatically set. Is used internally, so don't pass this yourself.
 */
export type Tokenize<
  Input extends string,
  Tokens extends Token[] = []
> = SkipWhiteSpace<Input> extends infer Input extends string
  ? Input extends `${keyof StaticTokenMap}${string}`
    ? ReadKeyword<Input> extends ReadResult<
        infer Rest,
        infer Keyword extends keyof StaticTokenMap
      >
      ? Tokenize<Rest, [...Tokens, StaticTokenMap[Keyword]]>
      : never
    : IsLetter<Input> extends true
    ? ReadIdentifier<Input> extends ReadResult<infer Rest, infer Identifier>
      ? Tokenize<Rest, [...Tokens, Token<TokenType.Ident, Identifier>]>
      : never
    : Input extends `${bigint}${string}`
    ? ReadInt<Input> extends ReadResult<infer Rest, infer Integer>
      ? Tokenize<Rest, [...Tokens, Token<TokenType.Int, Integer>]>
      : never
    : Input extends ""
    ? [...Tokens, StaticTokenMap["\0"]]
    : [...Tokens, IllegalToken<Input>]
  : never;

/**
 * Tokenizer to align with other implementations. If possible use {@link Tokenize} instead as it is faster
 * than using {@link GetNextToken}.
 *
 * @template Input the input source code
 * @template Tokens internal to the implementation. Do not pass yourself. This stores all tokens using {@link Tokenize}.
 * @template Position internal to the implementation. Do not pass yourself. This stores the index imside the {@code Tokens} array using a tuple's length.
 * @template CurrentToken internal to the implementation. Do not pass yourself. This stores the last read token.
 */
export type Tokenizer<
  Input extends string = string,
  Tokens extends Token[] = string extends Input ? Token[] : Tokenize<Input>,
  Position extends 1[] = string extends Input ? 1[] : [],
  CurrentToken extends Token = string extends Input ? Token | never : never
> = {
  input: Input;
  tokens: Tokens;
  position: Position;
  token: CurrentToken;
};

/**
 * GetNextToken function to align with other implementations. If possible use {@link Tokenize} instead as it is faster.
 *
 * @template TTokenizer an instance of {@link Tokenizer}.
 * @returns a new instance of {@link Tokenizer} taht reads the next token ({@link Tokenizer.token}).
 */
export type GetNextToken<TTokenizer extends Tokenizer> = Tokenizer<
  TTokenizer["input"],
  TTokenizer["tokens"],
  [...TTokenizer["position"], 1],
  TTokenizer["tokens"][TTokenizer["position"]["length"]]
>;

/**
 * helper type to get the current {@link Token} from a {@link Tokenizer}.
 * @template TTokenizer an instance of a tokenizer whose last rwad token to retreieve.
 */
export type GetToken<TTokenizer extends Tokenizer<string>> =
  TTokenizer["token"];
/**
 * @template IllegalLiteral the token that was illegal
 */
type IllegalToken<IllegalLiteral extends string> = Token<
  TokenType.Illegal,
  IllegalLiteral
>;

/**
 * holds a mapping from source code characters to tokens.
 * For most tokens, this works as they are always the same.
 * Exceptions are {@link TokenType.Ident} and the like.
 */
type StaticTokenMap = {
  "\0": Token<TokenType.Eof, "\0">;
  "=": Token<TokenType.Assign, "=">;
  "+": Token<TokenType.Plus, "+">;
  "-": Token<TokenType.Dash, "-">;
  "!": Token<TokenType.Bang, "!">;
  "*": Token<TokenType.Asterisk, "*">;
  "/": Token<TokenType.ForwardSlash, "/">;
  "<": Token<TokenType.LessThan, "<">;
  ">": Token<TokenType.GreaterThan, ">">;
  "==": Token<TokenType.Equal, "==">;
  "!=": Token<TokenType.NotEqual, "!=">;
  ",": Token<TokenType.Comma, ",">;
  ";": Token<TokenType.Semicolon, ";">;
  "(": Token<TokenType.LParen, "(">;
  ")": Token<TokenType.RParen, ")">;
  "{": Token<TokenType.LSquirly, "{">;
  "}": Token<TokenType.RSquirly, "}">;
  fn: Token<TokenType.Function, "fn">;
  let: Token<TokenType.Let, "let">;
  true: Token<TokenType.True, "true">;
  false: Token<TokenType.False, "false">;
  if: Token<TokenType.If, "if">;
  else: Token<TokenType.Else, "else">;
  return: Token<TokenType.Return, "return">;
};

type Whitespace = " " | "\t" | "\r" | "\n";

type SkipWhiteSpace<Input extends string> =
  Input extends `${Whitespace}${infer Acc}` ? SkipWhiteSpace<Acc> : Input;

type FirstChar<T extends string> = T extends `${infer Char}${string}`
  ? Char
  : never;

type StartsWith<
  T extends string,
  Prefix extends string
> = T extends `${Prefix}${infer Rest}`
  ? Rest extends ""
    ? never
    : Rest
  : never;
/** works because letters have both uppercase versions and lowercase versions */
type IsLetter<T extends string> = T extends `${infer Char}${string}`
  ? Uppercase<Char> extends Lowercase<Char>
    ? false
    : true
  : false;

/* these types are necessary, because something linke <code>`${"!=" | "!"}${string}`</code> only matches "!" and not "!=". */

type ReadIdentifier<
  Input extends string,
  Identifier extends string = ""
> = Input extends `${infer Char}${infer Rest}`
  ? IsLetter<Char> extends true
    ? ReadIdentifier<Rest, `${Identifier}${Char}`>
    : ReadResult<Input, Identifier>
  : ReadResult<Input, Identifier>;

type ReadKeyword<
  Input extends string,
  Keyword extends string = "",
  Keywords extends string = keyof StaticTokenMap
> = Input extends `${infer Char extends FirstChar<Keywords>}${infer Rest}`
  ? ReadKeyword<Rest, `${Keyword}${Char}`, StartsWith<Keywords, Char>>
  : ReadResult<Input, Keyword>;

type ReadInt<
  Input extends string,
  Number extends string = ""
> = Input extends `${infer Digit extends bigint}${infer Rest}`
  ? ReadInt<Rest, `${Number}${Digit}`>
  : ReadResult<Input, Number>;

type ReadResult<Rest extends string, ReadWord extends string> = [
  Rest,
  ReadWord
];
