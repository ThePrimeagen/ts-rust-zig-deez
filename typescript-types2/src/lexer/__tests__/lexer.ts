import { GetNextToken, GetToken, Token, TokenType, Tokenizer } from "..";

export type Assert<T extends true> = T;
export type Equal<X, Y> = (<T>() => T extends X ? 1 : 2) extends <
  T
>() => T extends Y ? 1 : 2
  ? true
  : false;

/**
 * check that the first {@code TokenTypes.length} tokens' types from {@code Lexer} match the passed {@code TokenTypes}.
 *
 * @template Lexer a Lexer instance
 * @template TokenTypes the token types to match
 */
export type CheckTokenTypes<
  T extends Tokenizer,
  TokenTypes extends TokenType[]
> = TokenTypes["length"] extends 0
  ? true
  : GetNextToken<T> extends infer T extends Tokenizer
  ? Equal<GetToken<T>["type"], TokenTypes[0]> extends true
    ? CheckTokenTypes<
        T,
        TokenTypes extends [TokenType, ...infer Rest extends TokenType[]]
          ? Rest
          : []
      >
    : {
        expected: TokenTypes[0];
        got: GetToken<T>;
      }
  : "next token not a Tokenizer";

/**
 * check that the first {@code Tokens.length} tokens from {@code Lexer} match the passed {@code Tokens}.
 *
 * @template Lexer a Lexer instance
 * @template Tokens the tokens to match
 */
export type CheckTokens<
  Lexer extends Tokenizer,
  Tokens extends Token[]
> = Tokens["length"] extends 0
  ? true
  : GetNextToken<Lexer> extends infer T extends Tokenizer
  ? Equal<GetToken<T>, Tokens[0]> extends true
    ? CheckTokens<
        T,
        Tokens extends [Token, ...infer Rest extends Token[]] ? Rest : []
      >
    : {
        expected: Tokens[0];
        got: GetToken<T>;
      }
  : "Next token not a Tokenizer";

/** test GetNextToken */
export namespace TestGetNextTokenToken {
  type Input = `=+(){},;`;
  export type Lexer = Tokenizer<Input>;

  type Tokens = [
    TokenType.Assign,
    TokenType.Plus,
    TokenType.LParen,
    TokenType.RParen,
    TokenType.LSquirly,
    TokenType.RSquirly,
    TokenType.Comma,
    TokenType.Semicolon
  ];

  export type Result = Assert<CheckTokenTypes<Lexer, Tokens>>;
}

/** test GetNextToken complete */
export namespace TestGetNextTokenComplete {
  type Input = `let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        `;
  type Lexer = Tokenizer<Input>;

  type Tokens = [
    { type: TokenType.Let; literal: "let" },
    { type: TokenType.Ident; literal: "five" },
    { type: TokenType.Assign; literal: "=" },
    { type: TokenType.Int; literal: "5" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Let; literal: "let" },
    { type: TokenType.Ident; literal: "ten" },
    { type: TokenType.Assign; literal: "=" },
    { type: TokenType.Int; literal: "10" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Let; literal: "let" },
    { type: TokenType.Ident; literal: "add" },
    { type: TokenType.Assign; literal: "=" },
    { type: TokenType.Function; literal: "fn" },
    { type: TokenType.LParen; literal: "(" },
    { type: TokenType.Ident; literal: "x" },
    { type: TokenType.Comma; literal: "," },
    { type: TokenType.Ident; literal: "y" },
    { type: TokenType.RParen; literal: ")" },
    { type: TokenType.LSquirly; literal: "{" },
    { type: TokenType.Ident; literal: "x" },
    { type: TokenType.Plus; literal: "+" },
    { type: TokenType.Ident; literal: "y" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.RSquirly; literal: "}" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Let; literal: "let" },
    { type: TokenType.Ident; literal: "result" },
    { type: TokenType.Assign; literal: "=" },
    { type: TokenType.Ident; literal: "add" },
    { type: TokenType.LParen; literal: "(" },
    { type: TokenType.Ident; literal: "five" },
    { type: TokenType.Comma; literal: "," },
    { type: TokenType.Ident; literal: "ten" },
    { type: TokenType.RParen; literal: ")" },
    { type: TokenType.Semicolon; literal: ";" },

    { type: TokenType.Bang; literal: "!" },
    { type: TokenType.Dash; literal: "-" },
    { type: TokenType.ForwardSlash; literal: "/" },
    { type: TokenType.Asterisk; literal: "*" },
    { type: TokenType.Int; literal: "5" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Int; literal: "5" },
    { type: TokenType.LessThan; literal: "<" },
    { type: TokenType.Int; literal: "10" },
    { type: TokenType.GreaterThan; literal: ">" },
    { type: TokenType.Int; literal: "5" },
    { type: TokenType.Semicolon; literal: ";" },

    { type: TokenType.If; literal: "if" },
    { type: TokenType.LParen; literal: "(" },
    { type: TokenType.Int; literal: "5" },
    { type: TokenType.LessThan; literal: "<" },
    { type: TokenType.Int; literal: "10" },
    { type: TokenType.RParen; literal: ")" },
    { type: TokenType.LSquirly; literal: "{" },
    { type: TokenType.Return; literal: "return" },
    { type: TokenType.True; literal: "true" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.RSquirly; literal: "}" },
    { type: TokenType.Else; literal: "else" },
    { type: TokenType.LSquirly; literal: "{" },
    { type: TokenType.Return; literal: "return" },
    { type: TokenType.False; literal: "false" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.RSquirly; literal: "}" },

    { type: TokenType.Int; literal: "10" },
    { type: TokenType.Equal; literal: "==" },
    { type: TokenType.Int; literal: "10" },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Int; literal: "10" },
    { type: TokenType.NotEqual; literal: "!=" },
    { type: TokenType.Int; literal: "9" },
    { type: TokenType.Semicolon; literal: ";" },

    { type: TokenType.Eof; literal: "\0" }
  ];
  export type Result = Assert<CheckTokens<Lexer, Tokens>>;
}
