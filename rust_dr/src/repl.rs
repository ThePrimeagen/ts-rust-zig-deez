use std::io::{self, Write};

use crate::lexer::lexer_nom::Lexer;
use crate::lexer::lexer_nom::Token;

pub fn repl() {
    let mut input = String::new();
    println!("Welcome to the Monkey REPL!");
    println!("Feel free to type in commands");
    
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        let mut lex = Lexer::new(input.as_str());
        let mut tok = lex.next_token().unwrap();
        while tok != Token::Eof {
            println!("{}", tok);
            tok = lex.next_token().unwrap();
        }
        input.clear();
    }
}
