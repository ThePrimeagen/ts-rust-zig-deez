# Lexical Interpreter Groovy Monkeylang Analyzer

## Dependencies
### Debian based
```bash
sudo apt install groovy make
```
### Arch btw
```bash
sudo pacman -S groovy
```
### MacOS
```bash
brew install groovy
```
## Running tests
```bash
# Run all tests
make test
```
Or
```bash
# Only lexer complete ATM
make test-lexer
```

## Docker (run this if you don't want to install any dependencies)
```bash
make docker-build
```
