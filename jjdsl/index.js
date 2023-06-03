function isSingleWidthToken(x) {
    if (x === "=" || x === "+" || x === "," || x === ";" || x === "(" || x === ")" || x === "{" || x === "}") {
        return new Token(x, undefined);
    } else {
        return null;
    }
}