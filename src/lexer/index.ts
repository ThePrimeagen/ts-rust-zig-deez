
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

