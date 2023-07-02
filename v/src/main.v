module main

import lexer { new_lexer }
import token { Token, TokenType }
import os

fn main() {
	for {
		example := os.input('> ')
		mut lex := new_lexer(example)

		for t := lex.next_token(); t.@type != TokenType.eof; t = lex.next_token() {
			println('${t.@type} ${t.value}')
		}
	}
}
