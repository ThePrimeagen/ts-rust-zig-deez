use anyhow::Result;
use ts_rust_zig_deez::lexer::lexer::{Lexer, Token};

fn main() -> Result<()> {

    std::io::stdin().lines().for_each(|line| {
        if let Ok(line) = line {
            let mut tokenizer = Lexer::new(line);

            while let Ok(token) = tokenizer.next_token() {
                println!("{} ", token);
                if let Token::Eof = token {
                    break;
                }
            }
        }
    });
    return Ok(());
}
