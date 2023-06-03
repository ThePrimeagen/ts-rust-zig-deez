// Classes

function Lexer(input) {
    this.input = input;
    this.position = 0;
    this.ch = this.input[this.position];
}

function Token(tokenType, value) {
    this.tokenType = tokenType;
    this.value = value;
}