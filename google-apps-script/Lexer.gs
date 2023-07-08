const TokenType = {
    Illegal: "ILLEGAL",
    Eof: "EOF",
    Ident: "IDENT",
    If: "if",
    Return: "return",
    True: "true",
    False: "false",
    Else: "else",
    Int: "INT",
    Assign: "=",
    NotEqual: "!=",
    Equal: "==",
    Plus: "+",
    Comma: ",",
    Semicolon: ";",
    LParen: "(",
    RParen: ")",
    LSquirly: "{",
    RSquirly: "}",
    Function: "FUNCTION",
    Let: "LET",
    Bang: "!",
    Dash: "-",
    ForwardSlash: "/",
    Asterisk: "*",
    LessThan: "<",
    GreaterThan: ">",
};

function createToken(type, literal) {
    return { type, literal };
}

const _0 = "0".charCodeAt(0);
const _9 = "9".charCodeAt(0);

const a = "a".charCodeAt(0);
const z = "z".charCodeAt(0);

const A = "A".charCodeAt(0);
const Z = "Z".charCodeAt(0);

const _ = "_".charCodeAt(0);

function isLetter(character) {
    const char = character.charCodeAt(0);
    return a <= char && z >= char || A <= char && Z >= char || char === _;
}

function isNumber(character) {
    const char = character.charCodeAt(0);
    return _0 <= char && _9 >= char;
}

const Keywords = {
    "fn": createToken(TokenType.Function, "fn"),
    "let": createToken(TokenType.Let, "let"),
    "return": createToken(TokenType.Return, "return"),
    "true": createToken(TokenType.True, "true"),
    "false": createToken(TokenType.False, "false"),
    "if": createToken(TokenType.If, "if"),
    "else": createToken(TokenType.Else, "else"),
};

class Tokenizer {
    
    constructor(input) {
        this.position = 0;
        this.readPosition = 0;
        this.ch = undefined;
        this.input = input;
        this.readChar();
    }

    getNextToken() {
        this.skipWhitespace();

        let tok;
        switch (this.ch) {
            case "{":
                tok = createToken(TokenType.LSquirly, this.ch);
                break;
            case "}":
                tok = createToken(TokenType.RSquirly, this.ch);
                break;
            case "(":
                tok = createToken(TokenType.LParen, this.ch);
                break;
            case ")":
                tok = createToken(TokenType.RParen, this.ch);
                break;
            case ",":
                tok = createToken(TokenType.Comma, this.ch);
                break;
            case "!":
                if (this.peek() === "=") {
                    this.readChar();
                    tok = createToken(TokenType.NotEqual, "!=");
                } else {
                    tok = createToken(TokenType.Bang, this.ch);
                }
                break;
            case ">":
                tok = createToken(TokenType.GreaterThan, this.ch);
                break;
            case "<":
                tok = createToken(TokenType.LessThan, this.ch);
                break;
            case "*":
                tok = createToken(TokenType.Asterisk, this.ch);
                break;
            case "/":
                tok = createToken(TokenType.ForwardSlash, this.ch);
                break;
            case "-":
                tok = createToken(TokenType.Dash, this.ch);
                break;
            case ";":
                tok = createToken(TokenType.Semicolon, this.ch);
                break;
            case "+":
                tok = createToken(TokenType.Plus, this.ch);
                break;
            case "=":
                if (this.peek() === "=") {
                    this.readChar();
                    tok = createToken(TokenType.Equal, "==");
                } else {
                    tok = createToken(TokenType.Assign, this.ch);
                }
                break;
            case "\0":
                tok = createToken(TokenType.Eof, "eof");
                break;
        }

        if (isLetter(this.ch)) {
            const ident = this.readIdent();
            const keyword = Keywords[ident];
            if (keyword) {
                return keyword;
            } else {
                return createToken(TokenType.Ident, ident);
            }
        } else if (isNumber(this.ch)) {
            return createToken(TokenType.Int, this.readInt());
        } else if (!tok) {
            return createToken(TokenType.Illegal, this.ch);
        }

        this.readChar();
        return tok;
    }

    peek() {
        if (this.readPosition >= this.input.length) {
            return "\0";
        } else {
            return this.input[this.readPosition];
        }
    }

    skipWhitespace() {
        while (this.ch === " " || this.ch === "\t" || this.ch === "\n" || this.ch === "\r") {
            this.readChar();
        }
    }

    readChar() {
        if (this.readPosition >= this.input.length) {
            this.ch = "\0";
        } else {
            this.ch = this.input[this.readPosition];
        }

        this.position = this.readPosition;
        this.readPosition += 1;
    }

    readIdent() {
        const position = this.position;

        while (isLetter(this.ch)) {
            this.readChar();
        }

        return this.input.slice(position, this.position);
    }

    readInt() {
        const position = this.position;

        while (isNumber(this.ch)) {
            this.readChar();
        }

        return this.input.slice(position, this.position);
    }
}
