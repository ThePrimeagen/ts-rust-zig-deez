Token.prototype.softCheck = function(x) {
    if (this.tokenType !== x.tokenType) {
        return false
    }
    return this.value === x.value;
}

Token.prototype.readable = function() {
    return "TokenType: " + this.tokenType + " | Value: " + this.value;
}
