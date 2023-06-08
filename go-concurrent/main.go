package main

import (
	"fmt"
	"monkeylang/lexer"
)

func main() {
	input := `let five = 5;
	let ten = 10;
	let add = fn(x, y) {
		x + y;
	};
	let result = add(five, ten);
	!-/*5;
	5 < 10 > 5;

	if (5 < 10) {
		return true;
	} else {
		return false;
	}

	10 == 10;
	10 != 9;`

	_, tokens := lexer.NewLexer(input)

	for t := range tokens {
		fmt.Println(t)
	}
}
