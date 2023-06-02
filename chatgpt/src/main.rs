use chatgpt::types::CompletionResponse;
use chatgpt::prelude::*;
use serde::Deserialize;
use serde_json::Result;
use std::process;
use std::env;
use dotenv;

#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct Token {
    tokentype: String,
    value: String,
}

async fn gippety_lexer(code: &str, api_key: &str) -> Vec<Token> {
    let client = ChatGPT::new(api_key).unwrap_or_else(|_| {
        println!("I don't know how to handle errors in Rust...");
        process::exit(69);
    });

    let response: CompletionResponse = client
        .send_message("Convert the following code into a list of tokens, based on the following available tokens:\n\nIdent(String),\nInt(String),\nIllegal,\nEof(Eof),\nEqual,\nPlus,\nComma,\nSemicolon,\nLParen,\nRParen,\nLBrace,\nRBrace,\nFunction,\nLet,\n\n```\n".to_string() + code + "\n```\n\nFormat your response as a JSON array: [{tokentype: \"Plus\", value: \"+\"}, ..., {tokentype: \"Eof\", value: \"Eof\"}]").await.unwrap_or_else(|_| {
            println!("I guess we should use anyhow or something...");
            process::exit(420);
        });

    let tokens: Vec<Token> = serde_json::from_str( &response.message().content ).expect( "ChatGPT knows what it's doing" );

    return tokens;
}

#[tokio::main]
async fn main() -> Result<()> {
    let api_key = dotenv::var("API_KEY").unwrap_or_else(|_| {
        println!("You need to put your OpenAI API key in .env");
        process::exit(69);
    });

    let file = env::args().nth(1).unwrap_or_else(|| {
        println!("ERROR: No input file provided");
        process::exit(420);
    });

    let code = std::fs::read_to_string(&file).unwrap_or_else(|_| {
        println!("Can't read the freaking file!");
        process::exit(1);
    });

    println!("Tokenizing your code. Wait a second (or 20)...");

    let tokens = gippety_lexer(&code, &api_key).await;

    println!("Here you go:\n{:?}", tokens);

    Ok(())
}
