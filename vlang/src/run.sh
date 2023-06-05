#!/bin/bash

# Run tests
echo "Running tests..."
v -o lexer_test lexer_test.v
./lexer_test

# Run lexer.v
echo "Running lexer.v..."
v lexer.v

echo "Done."
