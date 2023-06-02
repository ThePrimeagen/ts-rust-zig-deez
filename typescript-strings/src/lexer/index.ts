// Most tokens can be identified from their literals with a single string
// comparison, so just use the literals as the token type.
// Less code and fewer allocations, and no crazy typescript.

export type Token = string;

export function tokenIsInt(token: Token): boolean {
    return isNumber(token[0]);
}

// O(1) and compile time error if expecting is not in the valid set of keywords.
export function tokenIsKeyword(token: Token, expecting: Keyword): token is Keyword {
    return token === expecting;
}

// O(1) and compile time error if expecting is not in the valid set of punctuatin.
export function tokenIsPunctuation(token: Token, expecting: Punctuation): token is Punctuation {
    return token === expecting;
}

export function tokenIsIdentifier(token: Token): boolean {
    return isLetter(token[0]) && !isKeyword(token);
}

export function tokenIsIllegal(token: Token): boolean {
    return token === "@ILLEGAL";
}

const keywords = ["fn", "let", "return", "true", "false", "if", "else"] as const;
export type Keyword = typeof keywords[number];

export function isKeyword(word: string): word is Keyword {
    // In many other programming languages, I expect switch statements or hash
    // tables to be faster.  Not in javascript.
    return keywords.includes(word as Keyword);
}

// Punctuation that can be identified from a single character.
const unambiguousPunctuation = ["{", "}", "(", ")", ",", ">", "<",
    "*", "/", "-", ";", "+"] as const;
type UnambiguousPunctuaion = typeof unambiguousPunctuation[number];

// The full set of punctuation.
const punctuation = ["!", "!=", "=", "==", ...unambiguousPunctuation] as const;
export type Punctuation = typeof punctuation[number];

const _0 = "0".charCodeAt(0);
const _9 = "9".charCodeAt(0);

const a = "a".charCodeAt(0);
const z = "z".charCodeAt(0);

const A = "A".charCodeAt(0);
const Z = "Z".charCodeAt(0);

const _ = "_".charCodeAt(0);

function isLetter(character: string): boolean {
    const char = character.charCodeAt(0);
    return a <= char && z >= char || A <= char && Z >= char || char === _;
}

function isNumber(character: string): boolean {
    const char = character.charCodeAt(0);
    return _0 <= char && _9 >= char;
}

export class Tokenizer {
    private position: number = 0;
    private readPosition: number = 0;
    private ch!: string;
    constructor(private input: string) {
        this.readChar();
    }

    public getNextToken(): Token {
        this.skipWhitespace();

        let tok: Token | undefined;
        if (unambiguousPunctuation.includes(this.ch as UnambiguousPunctuaion)) {
            tok = this.ch;
        } else {
            switch (this.ch) {
                case "\0":
                        tok = this.ch;
                    break;
                case "!":
                    if (this.peek() === "=") {
                        this.readChar();
                        tok = "!=";
                    } else {
                        tok = this.ch;
                    }
                    break;
                case "=":
                    if (this.peek() === "=") {
                        this.readChar();
                        tok = "==";
                    } else {
                        tok = this.ch;
                    }
                    break;
            }
        }

        if (isLetter(this.ch)) {
            return this.readIdent();
        } else if (isNumber(this.ch)) {
            return this.readInt();
        } else if (!tok) {
            return "@ILLEGAL";
        }

        this.readChar();
        return tok as Token;
    }

    private peek(): string {
        if (this.readPosition >= this.input.length) {
            return "\0";
        } else {
            return this.input[this.readPosition];
        }
    }

    private skipWhitespace(): void {
        while (this.ch === " " || this.ch === "\t" || this.ch === "\n" || this.ch === "\r") {
            this.readChar();
        }
    }

    private readChar(): void {
        if (this.readPosition >= this.input.length) {
            this.ch = "\0";
        } else {
            this.ch = this.input[this.readPosition];
        }

        this.position = this.readPosition;
        this.readPosition += 1;
    }

    private readIdent(): string {
        const position = this.position;

        while (isLetter(this.ch)) {
            this.readChar();
        }

        return this.input.slice(position, this.position);
    }

    private readInt(): string {
        const position = this.position;

        while (isNumber(this.ch)) {
            this.readChar();
        }

        return this.input.slice(position, this.position);
    }
}
