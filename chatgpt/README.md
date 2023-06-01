# ChatGPT Tokenizer

Code tokenizer using the proompt engineering programming language.

## Quick Start

First you need to put your [OpenAI API key](https://platform.openai.com/account/api-keys) to the `.env` file:
```console
$ echo 'API_KEY="YOUR_OPEN_AI_API_KEY"' > .env
```

To test the tokenizer with the code in `test-code.txt`:
```console
$ make docker-ready
```

To tokenize your own code:
```console
$ cargo run [INPUT_FILE]
```
