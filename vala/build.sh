#!/usr/bin/env bash

set -xe

rm -f src/*.c monkey-repl monkey-test

VALAC_FLAGS="--save-temps --Xcc -Wno-incompatible-pointer-types"

valac \
  $VALAC_FLAGS \
  -o monkey-repl \
  src/lexer.vala \
  src/main.vala \
  src/repl.vala \
  src/token.vala

valac \
  $VALAC_FLAGS \
  -o monkey-test \
  src/lexer.vala \
  src/test.vala \
  src/token.vala
