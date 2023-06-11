import { TokenType, Tokenize, Tokenizer } from "..";

export type Assert<T extends true> = T;
export type Equal<X, Y> = (<T>() => T extends X ? 1 : 2) extends <
  T
>() => T extends Y ? 1 : 2
  ? true
  : false;

/** test GetNextToken */
export namespace TestGetNextTokenToken {
  type Input = `=+(){},;`;
  export type Tokenized = Tokenize<Input>;

  type Tokens = [
    { type: TokenType.Assign; literal: "=" },
    { type: TokenType.Plus; literal: "+" },
    { type: TokenType.LParen; literal: "(" },
    { type: TokenType.RParen; literal: ")" },
    { type: TokenType.LSquirly; literal: "{" },
    { type: TokenType.RSquirly; literal: "}" },
    { type: TokenType.Comma; literal: "," },
    { type: TokenType.Semicolon; literal: ";" },
    { type: TokenType.Eof; literal: "\0" }
  ];

  export type Result = Assert<Equal<Tokenized, Tokens>>;
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
  type Tokenized = Tokenize<Input>;

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
  export type Result = Assert<Equal<Tokenized, Tokens>>;
}
