import { Token, Tokenizer } from "./lexer";

import readline from "readline";

const rs = readline.createInterface({
    input: process.stdin,
});

rs.on("line", (input) => {
    const tokenizer = new Tokenizer(input);

    while (true) {
        const token = tokenizer.getNextToken()
        console.log(token);
        if (token.type === "EOF") {
            break;
        }
    }
});

rs.on("close", () => {
    console.log("suck it chat");
});


