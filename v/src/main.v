module main

import lexer
import token { Token, TokenType }
import os

fn main() {
	for {
		example := os.input('> ')
		mut lex := lexer.Lexer.new(example)

		for t := lex.next_token(); t.@type != TokenType.eof; t = lex.next_token() {
			println('${t.@type} ${t.value}')
		}
	}
}
