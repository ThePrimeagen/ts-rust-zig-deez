// @ts-check

/**
 * supported token types
 * @enum {string}
 */
const Tokens = /** @type {const} */ ({
    ILLEGAL: "ILLEGAL",
    EOF: "EOF",
    IDENT: "IDENT",
    INT: "INT",
    FUNCTION: "FUNCTION",
    LET: "LET",
    EQUAL: "=",
    PLUS: "+",
    COMMA: ",",
    SEMI: ";",
    LPAREN: "(",
    RPAREN: ")",
    LBRACE: "{",
    RBRACE: "}",
});

/** @typedef {typeof Tokens[keyof typeof Tokens]} TokenType */

/**
 * @typedef {Object} Token
 * @property {TokenType} type - type of token
 * @property {string} [value] - (optional) value of token
 */

/**
 * a set of all token type values used for quick lookup
 * @type {Set<string>}
 */
const tokenSet = new Set(Object.values(Tokens));

/** @type {Record<string, TokenType>} */
const keywords = {
    fn: Tokens.FUNCTION,
    let: Tokens.LET,
};

class Lexer {
    /**
     * create a new lexer
     * @param {string} input - the input string to tokenize
     */
    constructor(input) {
        /**
         * current position in the input string
         * @type {number}
         */
        this.position = 0;

        /**
         * the input string
         * @type {string}
         */
        this.input = input;

        /**
         * the current character
         * @type {string}
         */
        this.ch = this.input[this.position];
    }

    /**
     * gets the next token in the input string
     * @returns {TokenType | Token} the next token
     */
    nextToken() {
        this.skipWhitespace();

        /** @type {TokenType | Token | null} */
        let token = tokenSet.has(this.ch) ? /** @type TokenType */ (this.ch) : null;

        if (token === null) {
            if (this.isAlpha(this.ch)) {
                const ident = this.readIdent();
                token = this.lookupIdent(ident);
                return token;
            }

            if (this.isNumeric(this.ch)) {
                token = Tokens.INT;
                const intVal = this.readInt();
                return { type: token, value: intVal };
            }

            token = this.ch === "" ? Tokens.EOF : Tokens.ILLEGAL;
        }

        this.readChar();

        return token;
    }

    /**
     * read the next character in the input string
     * @returns {void}
     */
    readChar() {
        this.position++;
        this.ch = this.position >= this.input.length ? "" : this.input[this.position];
    }

    /**
     * skip any whitespace characters in the input string
     * @returns {void}
     */
    skipWhitespace() {
        while (this.ch === " " || this.ch === "\t" || this.ch === "\n" || this.ch === "\r") {
            this.readChar();
        }
    }

    /**
     * read an identifier from the input string
     * @returns {string} the identifier
     */
    readIdent() {
        const pos = this.position;
        while (this.isAlpha(this.ch)) {
            this.readChar();
        }
        return this.input.slice(pos, this.position);
    }

    /**
     * read an integer literal from the input string
     * @returns {string} integer literal
     */
    readInt() {
        const pos = this.position;
        while (this.isNumeric(this.ch)) {
            this.readChar();
        }
        return this.input.slice(pos, this.position);
    }

    /**
     * check if a character is a letter
     * @param {string} ch - the character to check
     * @returns {boolean} true if the character is a letter, false otherwise
     */
    isAlpha(ch) {
        return ch >= "a" && ch <= "z" || ch >= "A" && ch <= "Z" || ch === "_";
    }

    /**
     * check if a character is numeric
     * @param {string} ch - the character to check
     * @returns {boolean} true if the character is a digit, false otherwise
     */
    isNumeric(ch) {
        return ch >= "0" && ch <= "9";
    }

    /**
     * look up an identifier in the keywords table
     * @param {string} ident - the identifier to look up
     * @returns {TokenType | Token} the token type or token
     */
    lookupIdent(ident) {
        return keywords[ident] || { type: Tokens.IDENT, value: ident };
    }
}

module.exports = { tokenTypes: Tokens, Lexer };
