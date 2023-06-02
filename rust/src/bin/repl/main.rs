use monka::lex::{Lexer, Token};
use std::io::{self, Stdin, Stdout, Write};

const TITLE: &str = r#"
    =
  = = =
= = = =
= = = =  =
= = = = ==
=========
= monka 
======="#;

const HELP: &str = r#"
help:     prints this message
clear:    clears the screen
exit:     exits the repl
<source>: lexed and printed
"#;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();

    repl(stdin, stdout)
}

fn repl(stdin: Stdin, stdout: Stdout) -> io::Result<()> {
    println!("{TITLE}");
    println!("{HELP}");

    loop {
        let mut line = String::new();

        print!("> ");
        stdout.lock().flush()?;
        stdin.read_line(&mut line)?;
        line = line
            .chars()
            .filter(|char| *char != '\n' && *char != '\r')
            .collect();

        match line.as_str() {
            "help" => println!("{HELP}"),
            "clear" => clear_screen(),
            "exit" => {
                clear_screen();

                break;
            }
            source => {
                let mut lexer = Lexer::new(source.to_owned());

                while let Ok(token) = lexer.next_token() {
                    println!("{token:?}");

                    if token == Token::Eof {
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

fn clear_screen() {
    print!("{esc}c", esc = 27 as char);
}
