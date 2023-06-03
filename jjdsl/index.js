// Classes

function Lexer(input) {
    this.input = input;
}

function Token(tokenType, value) {
    this.tokenType = tokenType;
    this.value = value;
}