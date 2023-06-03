// This is here just so Docker can cache Cargo and we don't have
// to rebuild the crates.io index every time we change the code

use chatgpt::types::CompletionResponse;
use chatgpt::prelude::*;
use serde::Deserialize;
use serde_json::Result;
use std::process;
use std::env;
use dotenv;

#[tokio::main]
async fn main() -> Result<()> {
    Ok(())
}
