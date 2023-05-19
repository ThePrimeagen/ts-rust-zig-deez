
export const TokenType = {
    Illegal: "ILLEGAL",
    Eof: "EOF",
    Ident: "IDENT",
    Int: "INT",
    Equal: "=",
    Plus: "+",
    Comma: ",",
    Semi: ";",
    LParen: "(",
    RParen: ")",
    LSquirly: "{",
    RSquirly: "}",
    Function: "FUNCTION",
    Let: "LET",
} as const;

type TokenItem = typeof TokenType[keyof typeof TokenType];

export type Token = {
    type: TokenItem;
    literal: string;
}

export function createToken(type: TokenItem, literal: string): Token {
    return { type, literal };
}

export class Tokenizer {
    private position: number = 0;
    private readPosition: number = 0;
    private character: string;
    constructor(private input: string) {
        this.readChar();
    }

    /** @throws {Error} */
    public getNextToken(): Token {
        let tok: Token;
        switch (this.character) {
            case "{":
                tok = createToken(TokenType.LSquirly, this.character);
                break;
            case "}":
                tok = createToken(TokenType.RSquirly, this.character);
                break;
            case "(":
                tok = createToken(TokenType.LParen, this.character);
                break;
            case ")":
                tok = createToken(TokenType.RParen, this.character);
                break;
            case ",":
                tok = createToken(TokenType.Comma, this.character);
                break;
            case ";":
                tok = createToken(TokenType.Semi, this.character);
                break;
            case "+":
                tok = createToken(TokenType.Plus, this.character);
                break;
            case "=":
                tok = createToken(TokenType.Equal, this.character);
                break;
            case "\0":
                tok = createToken(TokenType.Eof, "");
                break;
            default:
                throw new Error("Unhandled token");
        }

        this.readChar();
        return tok;
    }

    private readChar(): void {
        if (this.readPosition >= this.input.length) {
            this.character = "\0";
        } else {
            this.character = this.input[this.readPosition];
        }

        this.position = this.readPosition;
        this.readPosition += 1;
    }
}
