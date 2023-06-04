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
	`
	_, tokens := lexer.NewLexer(input)

	for t := range tokens {
		fmt.Println(t)
	}
}
