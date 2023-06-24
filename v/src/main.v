module main

import lexer { Token, new_lexer }
import os

fn main() {
	for {
		example := os.input('> ')
		mut lex := new_lexer(example)

		for t, v := lex.next_token(); t != Token.eof; t, v = lex.next_token() {
			println('${t} ${v}')
		}
	}
}
