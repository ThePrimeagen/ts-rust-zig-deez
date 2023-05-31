#!/usr/bin/env bash

set -euo pipefail

source "$(dirname -- "${BASH_SOURCE[0]}")/lexer.sh"

ACTUAL=$(tokenize "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);")

EXPECTED="TOK_LET;
TOK_IDENT;five
TOK_EQ;
TOK_INT;5
TOK_SEMICOLON;
TOK_LET;
TOK_IDENT;ten
TOK_EQ;
TOK_INT;10
TOK_SEMICOLON;
TOK_LET;
TOK_IDENT;add
TOK_EQ;
TOK_FN;
TOK_LPAREN;
TOK_IDENT;x
TOK_COMMA;
TOK_IDENT;y
TOK_RPAREN;
TOK_LSQUIRLY;
TOK_IDENT;x
TOK_PLUS;
TOK_IDENT;y
TOK_SEMICOLON;
TOK_RSQUIRLY;
TOK_SEMICOLON;
TOK_LET;
TOK_IDENT;result
TOK_EQ;
TOK_IDENT;add
TOK_LPAREN;
TOK_IDENT;five
TOK_COMMA;
TOK_IDENT;ten
TOK_RPAREN;
TOK_SEMICOLON;
TOK_EOF;"

diff -u <(echo "$EXPECTED") <(echo "$ACTUAL")