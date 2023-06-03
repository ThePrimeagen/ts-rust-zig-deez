#!/bin/bash

# Install V
echo "Installing V..."
git clone https://github.com/vlang/v
cd v
make
sudo ./v symlink
cd ..
rm -rf v

# Run tests
echo "Running tests..."
v -o lexer_test lexer_test.v
./lexer_test

# Run lexer.v
echo "Running lexer.v..."
v lexer.v

echo "Done."
