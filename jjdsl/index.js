Lexer.prototype.readChar = function () {
    this.position++;
    this.ch = this.position >= this.input.length ? "" : this.input[this.position];
}

Lexer.prototype.isAlpha = function (x) {
    return x >= "a" && x <= "z" || x >= "A" && x <= "Z" || x === "_";
}

Lexer.prototype.isNumeric = function (x) {
    return x >= "0" && x <= "9";
}


Lexer.prototype.skipWhitespace = function () {
    while (this.ch === " " || this.ch === "\t" || this.ch === "\n" || this.ch === "\r") {
        this.readChar();
    }
}

Lexer.prototype.readIdent = function () {
    var pos = this.position;
    while (this.isAlpha(this.ch)) {
        this.readChar();
    }
    return this.input.slice(pos, this.position);
}

Lexer.prototype.readInt = function () {
    var pos = this.position;
    while (this.isNumeric(this.ch)) {
        this.readChar();
    }
    return this.input.slice(pos, this.position);
}

Lexer.prototype.lookUpIdent = function (x) {
    if (x === "fn") {
        return new Token("FUNCTION", undefined);
    } else if (x === "let") {
        return new Token("LET", undefined);
    } else {
        return new Token("IDENT", x);
    }
}

Lexer.prototype.nextToken = function () {
    this.skipWhitespace();

    var token = isSingleWidthToken(this.ch);

    if (token === null) {
        if (this.isAlpha(this.ch)) {
            var ident = this.readIdent();
            return this.lookUpIdent(ident);
        }
        if (this.isNumeric(this.ch)) {
            var intVal = this.readInt();
            return new Token("INT", intVal);
        }
        token = this.ch === "" ? new Token("EOF", "") : new Token("ILLEGAL", null);

    }

    this.readChar();

    return token;
}